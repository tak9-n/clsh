(defpackage clsh-jobs
  (:use common-lisp cffi)
  (:export
   create-job
   wait-job
   make-job-active
   get-first-job
   pick-finished-jobs))

(in-package clsh-jobs)

(defvar *clsh-pgid* (sb-posix:getpgrp))
(defvar *jobs* nil)
(defvar *last-done-job-status* nil)
(defvar *current-job* nil)

(sb-sys:ignore-interrupt sb-posix:sigttou)
(sb-sys:ignore-interrupt sb-posix:sigttin)
(sb-sys:ignore-interrupt sb-posix:sigtstp)

(defvar *tty-fd* (sb-posix:open #p"/dev/tty" sb-posix:o-rdwr))
(defun exec (program args)
  ;; thanks to JDz
  (cffi:foreign-funcall "execvp"
                        :string program
                        :pointer (cffi:foreign-alloc :string
                                                     :initial-contents `(,program ,@args)
                                                     :null-terminated-p t)
                        :int))
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

(defun run-program (cmd args &key (input t) (output t))
  (let ((pid (sb-posix:posix-fork)))
    (if (eq pid 0)
        (progn ;child
          (sb-posix:setpgrp)
          (tcsetpgrp *tty-fd* pid)
          (sb-sys:default-interrupt sb-posix:sigtstp)
          (sb-sys:default-interrupt sb-posix:sigttou)
          (sb-sys:default-interrupt sb-posix:sigttin)
          ;; (let ((input-fd (if (eq input t) 0 (sb-sys:fd-stream-fd input)))
          ;;       (output-fd (if (eq output t) 1 (sb-sys:fd-stream-fd output))))
          ;;   (sb-posix:dup2 input-fd 0)
          ;;   (sb-posix:dup2 output-fd 1))
          (close-all-fd)
          (exec cmd args))
        (progn ;parent
          (sb-posix:setpgid pid pid)
          (setf *current-job* pid)
          pid))))

(defun delete-done-job (job)
  (setf *jobs* (delete job *jobs*)))
#+sbcl
(defun create-job (cmd args input output)
  (let ((proc (run-program cmd args :output output :input input)))
    (push proc *jobs*)
    proc))
(defun show-status-message (job status)
  (format t "[~d] ~d~%"
          job
          (cond ((sb-posix:wifstopped status)
                 "stopped")
                (t
                 (format nil "status=~d" status)))))

#+sbcl
(defun pick-finished-job (job)
  (multiple-value-bind (pid status) (sb-posix:waitpid job sb-posix:wnohang)
    (when (and (eq pid job) (sb-posix:wifexited status))
      (show-status-message job status)
      (delete-done-job job))))
#+sbcl
(defun wait-job (job)
  (unless (eq *current-job* (sb-posix:getpid))
    (tcsetpgrp *tty-fd* job)
    (setf *current-job* job))
  (multiple-value-bind (pid status) (sb-posix:waitpid job sb-posix:wuntraced)
    (declare (ignore pid))
    (tcsetpgrp *tty-fd* *clsh-pgid*)
    (if (sb-posix:wifexited status)
        (progn
          (setf *last-done-job-status* status)
          (delete-done-job job))
        (show-status-message job status))
    (setf *current-job* nil)))
#+SBCL
(defun send-signal-to-job (job sig-no)
  (sb-posix:killpg job sig-no)
  (let ((jobs *jobs*))
    (setf jobs (delete job jobs))
    (push job jobs)
    (setf *jobs* jobs)))
(defun make-job-active (job foreground)
  (let ((target (if job job (car *jobs*))))
    (send-signal-to-job target
                        sb-posix:sigcont)
    (if foreground
        (wait-job target)
        (show-status-message job -1))))
(defun get-first-job ()
  (car *jobs*))
(defun pick-finished-jobs ()
  (mapc (lambda (job)
          (pick-finished-job job))
        *jobs*))
