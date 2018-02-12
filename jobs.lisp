(require :bordeaux-threads)

(defpackage clsh.jobs
  (:use common-lisp cffi bordeaux-threads)
  (:export
   create-job
   create-lisp-job
   wait-job
   make-job-active
   pick-finished-jobs
   show-jobs))

(in-package clsh.jobs)

(defgeneric wait-job (job))
(defgeneric make-job-obj-active (job foreground))
(defgeneric show-status-message (job))
(defgeneric pick-finished-job (job))
(defgeneric send-signal-to-job (job sig-no))

(defclass job
    ()
  ((no :initarg :no)
   (status :initarg :status)))

(defclass command-job
    (job)
  ((commands :initarg :commands)
   (pids :initarg :pids)
   (pgid :initarg :pgid)))

(defclass lisp-job
    (job)
  ((exp :initarg :exp)
   (thread :initarg :thread)
   result))

(defvar *jobno-counter* 1)

(defvar *clsh-pgid* (sb-posix:getpgrp))
(defvar *jobs* nil)
(defvar *last-done-job-status* nil)
(defvar *current-job* nil)

(sb-sys:ignore-interrupt sb-posix:sigttou)
(sb-sys:ignore-interrupt sb-posix:sigttin)
(sb-sys:ignore-interrupt sb-posix:sigtstp)

(defvar *tty-fd* (sb-posix:open #p"/dev/tty" sb-posix:o-rdwr))
(defun exec (program args)
  (let ((c-args (cffi:foreign-alloc :string
                                    :initial-contents `(,program ,@args)
                                    :null-terminated-p t)))
    (cffi:foreign-funcall "execv"
                          :string program
                          :pointer c-args
                          :int)
    (cffi:foreign-free c-args))
  (princ "not found command")
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

(defun run-programs (cmds &key (input 0) (output 1) (error 2))
  (do* ((now-cmds cmds (cdr now-cmds))
        (cmd (car now-cmds) (car now-cmds))
        (grpid nil)
        (next-in)
        (out-fd)
        (in-fd input next-in)
        (child-pids))
       ((null now-cmds) (values child-pids grpid))
    (if (cdr now-cmds)
        (multiple-value-bind (p-in p-out)
            (sb-posix:pipe)
          (setf out-fd p-out)
          (setf next-in p-in))
        (progn
          (setf out-fd output)
          (setf next-in nil)))
    (sb-thread::with-all-threads-lock
      (let ((pid (sb-posix:posix-fork)))
        (if (eq pid 0)
            (progn ;child
              #+darwin
              (sb-posix:darwin-reinit)
              (if grpid
                  (sb-posix:setpgid 0 grpid)
                  (progn
                    (sb-posix:setpgrp)
                    (tcsetpgrp *tty-fd* pid)))
              (sb-sys:default-interrupt sb-posix:sigtstp)
              (sb-sys:default-interrupt sb-posix:sigttou)
              (sb-sys:default-interrupt sb-posix:sigttin)
              (unless (eq in-fd input)
                (sb-posix:dup2 in-fd 0))
              (unless (eq out-fd output)
                (sb-posix:dup2 out-fd 1))
              (unless (eq out-fd error)
                (sb-posix:dup2 out-fd 2))
              (close-all-fd)
              (exec (car cmd) (cdr cmd)))
            (progn ;parent
              (unless grpid
                (setf grpid pid)
                (setf *current-job* pid))
              (sb-posix:setpgid pid grpid)
              (unless (eq in-fd input)
                (sb-posix:close in-fd))
              (unless (eq out-fd output)
                (sb-posix:close out-fd))
              (push pid child-pids)))))))

(defun delete-done-job (job)
  (setf *jobs* (delete job *jobs*))
  (unless *jobs*
    (setf *jobno-counter* 1)))

(defun register-job (job)
  (push job *jobs*)
  (setf *jobno-counter* (1+ *jobno-counter*)))

#+sbcl
(defun get-fd-from-stream (stream)
  (cond ((typep stream 'synonym-stream)
         (get-fd-from-stream (symbol-value (synonym-stream-symbol stream))))
        ((sb-sys:fd-stream-p stream)
         (sb-sys:fd-stream-fd stream))
        (t nil)))

#+sbcl
(defun create-job (cmds &key (input *standard-input*) (output *standard-output*) (error *error-output*))
  (let ((fds (mapcar (lambda (fs)
                       (get-fd-from-stream fs))
                     `(,input ,output ,error))))
    (if (some #'null fds)
        nil
        (multiple-value-bind (pids grpid)
            (run-programs cmds :input (first fds) :output (second fds) :error (third fds))
          (let ((proc (make-instance
                       'command-job
                       :no *jobno-counter*
                       :commands cmds
                       :pids pids
                       :pgid grpid
                       :status 'running)))
            (register-job proc)
            proc)))))

(defun create-lisp-job (exp &key (input *standard-input*) (output *standard-output*) (error *error-output*))
  (let* ((thr (make-thread (lambda ()
                             (eval exp))
                           :initial-bindings `((*standard-input* . ,input)
                                               (*standard-output* . ,output)
                                               (*error-output* . ,error))))
         (job (make-instance
               'lisp-job
               :no *jobno-counter*
               :exp exp
               :thread thr
               :status 'running)))
    (register-job job)
    job))

(defun status2string (status)
  (case status
    (running "running")
    (stopped "stopped")
    (otherwise
     (format nil "status=~a" status))))

(defmethod show-status-message ((job command-job))
  (with-slots (no status commands) job
    (format t "[~d] ~a ~{~{~a~}~^ | ~}~a~%"
            no
            (status2string status)
            commands
            (if (eq *current-job* job)
                ""
                " &"))))

(defmethod show-status-message ((job lisp-job))
  (with-slots (no status result) job
      (format t "[~d] ~a result: ~s ~a~%"
              no
              (status2string status)
              result
            (if (eq *current-job* job)
                ""
                " &"))))

#+sbcl
(defmethod pick-finished-job ((job command-job))
  (with-slots (pgid status) job
    (multiple-value-bind (pid status) (sb-posix:waitpid pgid sb-posix:wnohang)
      (when (and (eq pid pgid) (sb-posix:wifexited status))
        (setf status status)
        (show-status-message job)
        (delete-done-job job)))))

(defmethod pick-finished-job ((job lisp-job))
  (with-slots (thread status result) job
    (unless (thread-alive-p thread)
      (setf result (join-thread thread))
      (show-status-message job)
      (delete-done-job job))))

#+sbcl
(defmethod wait-job ((job command-job))
  (with-slots (pgid pids status) job
    (unless (eq *current-job* job)
      (tcsetpgrp *tty-fd* pgid)
      (setf *current-job* job))
    (multiple-value-bind (pid status) (sb-posix:waitpid (car pids) sb-posix:wuntraced)
      (declare (ignore pid))
      (tcsetpgrp *tty-fd* *clsh-pgid*)
      (if (sb-posix:wifexited status)
          (progn
            (setf *last-done-job-status* status)
            (delete-done-job job))
          (progn
            (setf status 'stopped)
            (show-status-message job)))
      (setf *current-job* nil))))

(defmethod wait-job ((job lisp-job))
  (with-slots (thread status result) job
    (setf *current-job* job)
    (setf result (join-thread thread))
    (setf status 'finished)
    (delete-done-job job)
    (show-status-message job))
  (setf *current-job* nil))

#+sbcl
(defmethod send-signal-to-job ((job command-job) sig-no)
  (with-slots (pgid) job
    (sb-posix:killpg pgid sig-no)
    (let ((jobs *jobs*))
      (setf jobs (delete job jobs))
      (push job jobs)
      (setf *jobs* jobs))))

;TODO need signal processing, like stopping, restart, and others
(defmethod send-signal-to-job ((job lisp-job) sig-no)
  (with-slots (thread) job
    (destroy-thread thread)))

(defun find-job-by-jobno (jobno)
  (find-if (lambda (job)
             (with-slots (no) job
               (eq no jobno)))
           jobno))

(defmethod make-job-obj-active ((job command-job) foreground)
  (with-slots (status) job
    (progn
      (send-signal-to-job job
                          sb-posix:sigcont)
      (setf status 'running)
      (if foreground
          (wait-job job)))))

(defmethod make-job-obj-active ((job lisp-job) foreground)
  (when foreground
    (with-slots (status) job
      (setf status 'running)
      (wait-job job))))

(defun make-job-active (jobno foreground)
  (let ((job (if jobno (find-job-by-jobno jobno) (car *jobs*))))
    (if job
        (make-job-obj-active job foreground)
        (format *error-output* "No such a job."))))

(defun pick-finished-jobs ()
  (mapc (lambda (job)
          (pick-finished-job job))
        *jobs*))

(defun show-jobs ()
  (mapc (lambda (job)
          (show-status-message job))
        *jobs*))
