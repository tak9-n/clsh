(defpackage clsh.jobs
  (:use common-lisp cffi)
  (:export
   create-job
   wait-job
   make-job-active
   pick-finished-jobs
   show-jobs))

(in-package clsh.jobs)

(defvar *jobno-counter* 1)
(defstruct job
  no
  commands
  pids
  pgid
  status)

(defvar *clsh-pgid* (sb-posix:getpgrp))
(defvar *jobs* nil)
(defvar *last-done-job-status* nil)
(defvar *current-job* nil)

(sb-sys:ignore-interrupt sb-posix:sigttou)
(sb-sys:ignore-interrupt sb-posix:sigttin)
(sb-sys:ignore-interrupt sb-posix:sigtstp)

(defvar *tty-fd* (sb-posix:open #p"/dev/tty" sb-posix:o-rdwr))
(defun exec (program args)
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

(defun run-programs (cmds &key (input 0) (output 1))
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
    (let ((pid (sb-posix:posix-fork)))
      (if (eq pid 0)
          (progn ;child
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
            (push pid child-pids))))))

(defun delete-done-job (job)
  (setf *jobs* (delete job *jobs*))
  (unless *jobs*
    (setf *jobno-counter* 1)))

#+sbcl
(defun create-job (cmds input output)
  (multiple-value-bind (pids grpid)
      (run-programs cmds :output output :input input)
    (let ((proc (make-job :no *jobno-counter*
                          :commands cmds
                          :pids pids
                          :pgid grpid
                          :status 'running)))
      (push proc *jobs*)
      (setf *jobno-counter* (1+ *jobno-counter*))
      proc)))

(defun status2string (status)
  (case status
    (running "running")
    (stopped "stopped")
    (otherwise
     (format nil "status=~a" status))))

(defun show-status-message (job)
  (format t "[~d] ~a ~{~{~a~}~^ | ~}~a~%"
          (job-no job)
          (status2string (job-status job))
          (job-commands job)
          (if (eq *current-job* job)
              ""
              " &")))

#+sbcl
(defun pick-finished-job (job)
  (multiple-value-bind (pid status) (sb-posix:waitpid (job-pgid job) sb-posix:wnohang)
    (when (and (eq pid (job-pgid job)) (sb-posix:wifexited status))
      (setf (job-status job) status)
      (show-status-message job)
      (delete-done-job job))))

#+sbcl
(defun wait-job (job)
  (unless (eq *current-job* job)
    (tcsetpgrp *tty-fd* (job-pgid job))
    (setf *current-job* job))
  (multiple-value-bind (pid status) (sb-posix:waitpid (car (job-pids job)) sb-posix:wuntraced)
    (declare (ignore pid))
    (tcsetpgrp *tty-fd* *clsh-pgid*)
    (if (sb-posix:wifexited status)
        (progn
          (setf *last-done-job-status* status)
          (delete-done-job job))
        (progn
          (setf (job-status job) 'stopped)
          (show-status-message job)))
    (setf *current-job* nil)))
#+SBCL
(defun send-signal-to-job (job sig-no)
  (sb-posix:killpg (job-pgid job) sig-no)
  (let ((jobs *jobs*))
    (setf jobs (delete job jobs))
    (push job jobs)
    (setf *jobs* jobs)))

(defun find-job-by-jobno (jobno)
  (find-if (lambda (job)
             (eq (job-no job) jobno))
           jobno))

(defun make-job-active (jobno foreground)
  (let ((job (if jobno (find-job-by-jobno jobno) (car *jobs*))))
    (if job
        (progn
          (send-signal-to-job job
                              sb-posix:sigcont)
          (setf (job-status job) 'running)
          (if foreground
              (wait-job job)))
        (format *error-output* "No such a job."))))

(defun pick-finished-jobs ()
  (mapc (lambda (job)
          (pick-finished-job job))
        *jobs*))
(defun show-jobs ()
  (mapc (lambda (job)
          (show-status-message job))
        *jobs*))
