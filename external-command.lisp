(defpackage clsh.external-command
  (:use common-lisp
        clsh.utils
        alexandria)
  (:export
   external-command-init
   *command-list*
   #:lookup-external-command
   #:run-external-command
   #:set-current-pgid
   #:build-command-hash
   #:make-proc))

(in-package clsh.external-command)

(defvar *command-list* nil)
(defvar *command-hash* nil)

(defvar *tty-fd*)
(defun external-command-init ()
  (sb-sys:enable-interrupt sb-posix:sigttou :ignore)
  (sb-sys:enable-interrupt sb-posix:sigttin :ignore)
  (sb-sys:enable-interrupt sb-posix:sigtstp :ignore)
  (sb-sys:enable-interrupt sb-posix:sigchld (lambda (signo info context)
                                              (declare (ignore signo info context))))
  (setf *tty-fd* (sb-posix:open #p"/dev/tty" sb-posix:o-rdwr)))

(defun exec (program args)
  (let ((c-args (cffi:foreign-alloc :string
                                    :initial-contents args
                                    :null-terminated-p t)))
    (cffi:foreign-funcall "execv"
                          :string program
                          :pointer c-args
                          :int)
    (cffi:foreign-free c-args))
  (format t "not found command ~a" program)
  (fresh-line)
  (sb-posix:exit 1))

(defun tcsetpgrp (fd pgrp)
  (cffi:foreign-funcall "tcsetpgrp"
                        :int fd
                        :int pgrp
                        :int))
(defun tcgetpgrp (fd)
  (cffi:foreign-funcall "tcgetpgrp"
                        :int fd
                        :int))
(defun getdtablesize ()
  (cffi:foreign-funcall "getdtablesize"
                        :int))

(defun close-all-fd ()
  (do ((fd (getdtablesize) (1- fd)))
      ((< fd 3))
    (handler-case
        (sb-posix:close fd)
      (sb-posix:syscall-error (e) (declare (ignore e))))))

(defun command-path-specified-p (cmd)
  (ppcre:scan "/" cmd))

(defun command-with-path (cmd)
  (if (command-path-specified-p cmd)
      cmd
      (let ((cmd-path (gethash cmd *command-hash* nil)))
        (if cmd-path
            (namestring cmd-path)
            nil))))

#+sbcl
(defun executable-p (path)
  (handler-case
      (progn (sb-posix:access (namestring path) sb-posix:x-ok)
             t)
    (sb-posix:syscall-error (e) (declare (ignore e)) nil)))

(defun build-command-hash ()
  (let ((paths (mapcar (lambda (ps) (make-pathname :directory ps :name :wild))
                       (ppcre:split ":" (sb-posix:getenv "PATH"))))
        (command-list))
    (setf *command-hash* (make-hash-table :test #'equal))
    (mapc (lambda (p)
            (mapc (lambda (f)
                    (when (executable-p f)
                      (when-let ((name (pathname-name f)))
                        (setf (gethash name *command-hash*) f)
                        (push name command-list))))
                  (directory p :resolve-symlinks nil)))
          paths)
    (setf *command-list* (sort-by-length command-list))))

(defun set-current-pgid (pgid)
  (tcsetpgrp *tty-fd* pgid))
#+sbcl
(defun exit ()
  (sb-posix:exit 0))

(defun make-proc (grpid task input output error)
  (let ((pid (sb-posix:posix-fork)))
    (if (eq pid 0)
        (progn ;child
          (unless (eq 0 input)
            (sb-posix:dup2 input 0))
          (unless (eq 1 output)
            (sb-posix:dup2 output 1))
          (unless (eq 2 error)
            (sb-posix:dup2 error 2))
          (close-all-fd)
          #+darwin
          (sb-posix:darwin-reinit)
          (if grpid
              (sb-posix:setpgid 0 grpid)
              (progn
                (sb-posix:setpgrp)
                (set-current-pgid pid)))
          (sb-sys:enable-interrupt sb-posix:sigtstp :default)
          (sb-sys:enable-interrupt sb-posix:sigttou :default)
          (sb-sys:enable-interrupt sb-posix:sigttin :default)
          (sb-sys:enable-interrupt sb-posix:sigint :default))
        (progn ;parent
          (unless grpid
            (setf grpid pid))
          (sb-posix:setpgid pid grpid)
          (unless (eq 0 input)
            (sb-posix:close input))
          (unless (eq 1 output)
            (sb-posix:close output))
          (unless (eq 2 error)
            (sb-posix:close error))
          (return-from make-proc pid))))
                                        ;in child
  (funcall task)
  (sb-posix:exit 0))

(defun expand-command-args (args)
  (reduce (lambda (result arg)
            (append result
                    (if (ppcre:scan "\\*" arg)
                        (mapcar #'namestring (if-let (paths (directory (pathname arg)))
                                               paths
                                               (return-from expand-command-args arg)))
                        (list arg))))
          args :initial-value nil))

(defun run-external-command (grpid cmd input output error)
  (make-proc grpid
             (lambda ()
               (exec (car cmd) cmd))
             input
             output
             error))

(defun lookup-external-command (cmd-spec)
  (let* ((cmd (car cmd-spec))
         (path-cmd (command-with-path cmd))
         (args (expand-command-args (cdr cmd-spec))))
    (when (typep args 'string)
      (format t "can't expand \"~a\"~%" args)
      (return-from lookup-external-command nil))
    (if (and path-cmd (executable-p path-cmd))
        (cons path-cmd args)
        (progn
          (format t "not found \"~a\" command~%" cmd)
          (return-from lookup-external-command nil)))))
