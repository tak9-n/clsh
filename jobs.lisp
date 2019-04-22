(defpackage clsh.jobs
  (:use common-lisp alexandria clsh.external-command cffi)
  (:export
   jobs-init
   create-job
   wait-job
   make-job-active
   pick-finished-jobs
   show-jobs))

(in-package clsh.jobs)

(defgeneric pick-finished-task (job))

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

(defvar *clsh-pgid*)
(defvar *jobs* nil)
(defvar *last-done-job-status* nil)
(defvar *current-job* nil)

#+sbcl
(defun getpgrp ()
  (sb-posix:getpgrp))

(defun jobs-init ()
  (setf *clsh-pgid* (getpgrp))
  (clsh.external-command:external-command-init))

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

#+sbcl
(defun eval-with-output (out-fd exp)
  (block eval-cmd
    (handler-bind
        ((error (lambda (e)
                  (format *error-output* "~a~%" e)
                  (sb-debug:print-backtrace)
                  (return-from eval-cmd))))
      (let ((os (sb-sys:make-fd-stream out-fd :output t))
            (result-sexp (eval (read-from-string exp))))
        (if result-sexp
            (format (if os t os) "~a~&" result-sexp)
            (fresh-line))))))

(defun create-lisp-task (grpid exp input output error)
  (let* ((task-pid (make-proc grpid
                              (lambda ()
                                (eval-with-output output exp))
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
  (multiple-value-bind (pid status) (sb-posix:waitpid (- pgid) sb-posix:wuntraced)
    (if (not (sb-posix:wifstopped status))
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
    (set-current-pgid (job-pgid job))
    (if (loop
           (let ((finished-pid (wait-pgid (job-pgid job))))
             (if finished-pid
                 (progn
                   (make-task-finished job (find-task-from-pid job finished-pid)))
                 (return nil))
             (unless executing-tasks
               (return t))))
        (progn
          (setf status 'finished
                *last-done-job-status* status)
          (delete-done-job job))
        (setf status 'stopped))
    (set-current-pgid *clsh-pgid*)
    (show-status-message job)
    (setf *current-job* nil)))

#+sbcl
(defun send-signal-to-job (job sig-no)
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

#+sbcl
(defconstant +sigcont+ sb-posix:sigcont)

(defun make-job-active (jobno foreground)
  (let ((job (if jobno (find-job-by-jobno jobno) (car *jobs*))))
    (if job
        (progn
          (set-current-pgid (job-pgid job))
          (send-signal-to-job job +sigcont+)
          (when foreground
            (wait-job job)))
        (format *error-output* "No such a job."))))

(defun pick-finished-jobs ()
  (mapc (lambda (job)
          (mapc
           (lambda (task)
             (when (pick-finished-task task)
               (make-task-finished job task)))
           (job-executing-tasks job))
          (unless (job-executing-tasks job)
            (show-status-message job)))
        *jobs*))

(defun show-jobs ()
  (mapc (lambda (job)
          (show-status-message job))
        *jobs*))

(defun parse-shell-to-lisp (shell-lst)
  (format nil "~s" shell-lst))

(defun check-command-executable (cmds)
  (let ((executable t))
    (values
     (mapcar (lambda (cmd)
               (case (clsh.parser:task-token-kind cmd)
                 (clsh.parser:shell
                  (let ((func-sym (clsh.utils:find-command-symbol (car (clsh.parser:task-token-task cmd)))))
                    (if (fboundp func-sym)
                        (cons 'clsh.parser:lisp
                              (parse-shell-to-lisp (cons func-sym (cdr (clsh.parser:task-token-task cmd)))))
                        (if-let (it (clsh.external-command:lookup-external-command (clsh.parser:task-token-task cmd)))
                          (cons 'clsh.parser:shell it)
                          (setf executable nil)))))
                 (otherwise
                  (cons 'clsh.parser:lisp (clsh.parser:task-token-task cmd)))
                 ))
             cmds)
     executable)))

#+sbcl
(defun pipe ()
  (multiple-value-bind (in out)
      (sb-posix:pipe)
    (cons in out)))

(defun create-job (cmds bg-flag &key (input *standard-input*) (output *standard-output*) (error *error-output*))
  (multiple-value-bind (trans-cmds result)
      (check-command-executable cmds)
    (destructuring-bind (in-s out-s err-s)
        (mapcar (lambda (fs)
                  (get-fd-from-stream fs))
                (list input output error))
      (if (and (= (length trans-cmds) 1) (eq (first (first trans-cmds)) 'clsh.parser:lisp))
          (eval-with-output out-s (cdr (first trans-cmds)))
          (let ((grpid nil))
            (when result
              (let ((job (make-job
                          :no *jobno-counter*
                          :executing-tasks
                          (do ((now-cmds trans-cmds (cdr now-cmds))
                               (before-cmd nil)
                               (in-fd in-s)
                               (result))
                              ((null now-cmds) result)
                            (let* ((after-cmd (cadr now-cmds))
                                   (cmd (first now-cmds))
                                   (pipe (if after-cmd
                                             (pipe)
                                             nil))
                                   (out-fd (if pipe
                                               (cdr pipe)
                                               out-s))
                                   (task (case (first cmd)
                                           (clsh.parser:lisp
                                            (create-lisp-task grpid (cdr cmd) in-fd out-fd err-s))
                                           (clsh.parser:shell
                                            (create-command-task grpid (cdr cmd) in-fd out-fd err-s))
                                           (otherwise
                                            (error "invalid token")))))
                              (unless grpid
                                (setf grpid (task-pid task)))
                              (setf before-cmd cmd)
                              (setf in-fd (car pipe))
                              (push task result)))
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
