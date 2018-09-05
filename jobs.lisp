(defpackage clsh.jobs
  (:use common-lisp alexandria clsh.external-command cffi)
  (:export
   create-job
   wait-job
   make-job-active
   pick-finished-jobs
   show-jobs))

(in-package clsh.jobs)

(defgeneric wait-task (job))
(defgeneric make-task-obj-active (job foreground))
(defgeneric pick-finished-task (job))
(defgeneric send-signal-to-task (job sig-no))

(defstruct job
  (no)
  (executing-tasks)
  (finished-tasks)
  (pgid)
  (status))

(defclass task ()
  ((pid :initarg :pid :accessor task-pid)))

(defclass command-task (task)
  ((command :initarg :command)
   (status :initarg :status)))

(defclass lisp-task (task)
  ((exp :initarg :exp)
   (result :initform nil)))

(defvar *jobno-counter* 1)

(defvar *clsh-pgid* (sb-posix:getpgrp))
(defvar *jobs* nil)
(defvar *last-done-job-status* nil)
(defvar *current-job* nil)

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
(defun create-command-task (grpid cmd-array input output error)
  (let* ((pid (run-external-command grpid cmd-array input output error))
         (task (make-instance
                'command-task
                :command cmd-array
                :pid pid)))
    task))

(defun create-lisp-task (grpid exp input output error)
  (let* ((task-pid (make-proc grpid
                              (lambda ()
                                (block eval-cmd
                                  (handler-bind
                                      ((error (lambda (e)
                                                (format *error-output* "~a~%" e)
                                                #+sbcl
                                                (sb-debug:print-backtrace)
                                                (return-from eval-cmd))))
                                    (eval (read-from-string exp)))))
                              input
                              output
                              error))
         (job (make-instance
               'lisp-task
               :exp exp
               :pid task-pid)))
    job))

(defun status2string (status)
  (case status
    (running "running")
    (stopped "stopped")
    (otherwise
     (format nil "status=~a" status))))

(defun show-status-message (job)
  (with-slots (no status finished-tasks) job
    (format t "[~d] ~a ~{~a~^ | ~}~a~%"
            no
            (status2string status)
            finished-tasks
            (if (eq *current-job* job)
                ""
                " &"))))

#+sbcl
(defmethod pick-finished-task ((task task))
  (with-slots (pid) task
    (multiple-value-bind (w-pid w-status) (sb-posix:waitpid pid sb-posix:wnohang)
      (declare (ignore w-pid))
      (sb-posix:wifexited w-status))))

#+sbcl
(defun wait-pgid (pgid)
  (set-current-pgid pgid)
  (multiple-value-bind (pid status) (sb-posix:waitpid (- pgid) sb-posix:wuntraced)
    (clsh.external-command:set-current-pgid *clsh-pgid*)
    (if (sb-posix:wifexited status)
        pid
        nil)))

(defun last-task (tasks)
  (first (reverse tasks)))

(defun first-task (tasks)
  (first tasks))

(defun find-task-from-pid (job pid)
  (find-if (lambda (task)
             (eq (task-pid task) pid))
           (job-executing-tasks job)))

(defun make-task-finished (job finished-task)
  (with-slots (executing-tasks finished-tasks in-fd out-fd err-fd) job
    (setf executing-tasks (delete finished-task executing-tasks))
    (push finished-task finished-tasks)))

(defun wait-job (job)
  (with-slots (status executing-tasks) job
    (setf *current-job* job)
    (if (loop
           (let ((finished-pid (wait-pgid (job-pgid job))))
             (if finished-pid
                 (make-task-finished job (find-task-from-pid job finished-pid))
                 (return nil))
             (unless executing-tasks
               (return t))))
        (progn
          (setf status 'finished
                *last-done-job-status* status)
          (delete-done-job job))
        (setf status 'stopped))
    (show-status-message job)
    (setf *current-job* nil)))

#+sbcl
(defmethod send-signal-to-task ((job task) sig-no)
  (with-slots (pgid) job
    (sb-posix:killpg pgid sig-no)
    (let ((jobs *jobs*))
      (setf jobs (delete job jobs))
      (push job jobs)
      (setf *jobs* jobs))))

(defun find-job-by-jobno (jobno)
  (find-if (lambda (job)
             (with-slots (no) job
               (eq no jobno)))
           jobno))

(defmethod make-task-obj-active ((job command-task) foreground)
  (with-slots (status) job
    (progn
      (send-signal-to-task (car job)
                          sb-posix:sigcont)
      (setf status 'running)
      (if foreground
          (wait-job job)))))

(defmethod make-task-obj-active ((job lisp-task) foreground)
  (when foreground
    (with-slots (status) job
      (setf status 'running)
      (wait-task job))))

(defun make-job-active (jobno foreground)
  (let ((job (if jobno (find-job-by-jobno jobno) (car *jobs*))))
    (if job
        (make-task-obj-active (car job) foreground)
        (format *error-output* "No such a job."))))

(defun pick-finished-jobs ()
  (mapc (lambda (job)
          (mapc
           (lambda (task)
             (when (pick-finished-task task)
               (make-task-finished job task)))
           job)
          (unless (job-executing-tasks job)
            (show-status-message job)))
        *jobs*))

(defun show-jobs ()
  (mapc (lambda (job)
          (show-status-message (car (reverse job))))
        *jobs*))

(defun parse-shell-to-lisp (shell-lst)
  (format nil "~s" shell-lst))

(defun check-command-executable (cmds)
  (let ((executable t))
    (values
     (mapcar (lambda (cmd)
               (case (first cmd)
                 (clsh.parser:shell
                  (let ((func-sym (clsh.utils:find-command-symbol (caadr cmd))))
                    (if (fboundp func-sym)
                        (list 'clsh.parser:lisp
                              (parse-shell-to-lisp (cons func-sym (cdadr cmd))))
                        (if-let (it (clsh.external-command:lookup-external-command cmd))
                          it
                          (setf executable nil)))))
                 (otherwise
                  cmd)
                 ))
             cmds)
     executable)))

#+sbcl
(defun pipe ()
  (sb-posix:pipe))

(defun create-job (cmds bg-flag &key (input *standard-input*) (output *standard-output*) (error *error-output*))
  (multiple-value-bind (trans-cmds result)
      (check-command-executable cmds)
    (if (and (= (length trans-cmds) 1) (eq (first (first trans-cmds)) 'clsh.parser:lisp))
        (eval (read-from-string (cadr (first trans-cmds))))
        (let ((grpid nil))
          (destructuring-bind (in-s out-s err-s)
              (mapcar (lambda (fs)
                        (get-fd-from-stream fs))
                      (list input output error))
            (when result
              (let ((job (make-job
                          :no *jobno-counter*
                          :executing-tasks
                          (mapcar (lambda (cmd)
                                    (let ((task (case (first cmd)
                                                  (clsh.parser:lisp
                                                   (create-lisp-task grpid (cadr cmd) in-s out-s err-s))
                                                  (clsh.parser:shell
                                                   (create-command-task grpid (cadr cmd) in-s out-s err-s))
                                                  (otherwise
                                                   (error "invalid token")))))
                                      (unless grpid
                                        (setf grpid (task-pid task)))
                                      task))
                                  trans-cmds)
                          :status 'running)))
                (setf (job-pgid job) grpid)
                (register-job job)
                (unless bg-flag
                  (wait-job job)))))))))

;; TODO 以下実装保留
;; (set-macro-character #\] (get-macro-character #\)))
;; (set-dispatch-macro-character #\# #\[
;;                               (lambda (stream char1 char2)
;;                                 (declare (ignore char1 char2))
;;                                 (setf (readtable-case *readtable*) :preserve)
;;                                 (unwind-protect
;;                                      (let ((command-line (read-delimited-list #\] stream t)))
;;                                        (list 'run-program-no-wait (princ-to-string (car command-line))
;;                                              `',(mapcar #'princ-to-string (rest command-line))
;;                                              ':output :stream))
;;                                   (setf (readtable-case *readtable*) :upcase))))
