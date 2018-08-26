(require :bordeaux-threads)

(defpackage clsh.jobs
  (:use common-lisp alexandria clsh.external-command cffi bordeaux-threads)
  (:export
   create-job
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
(defun create-command-task (cmd-array &key (input *standard-input*) (output *standard-output*) (error *error-output*))
  (let ((fds (mapcar (lambda (fs)
                       (get-fd-from-stream fs))
                     `(,input ,output ,error))))
    (if (some #'null fds)
        nil
        (let* ((pid (run-external-command cmd-array :input (first fds) :output (second fds) :error (third fds)))
               (proc (make-instance
                      'command-job
                      :no *jobno-counter*
                      :commands cmd-array
                      :pids (list pid)
                      :pgid pid ;todo
                      :status 'running)))
          (register-job proc)
          proc))))

(defun create-lisp-task (exp &key (input *standard-input*) (output *standard-output*) (error *error-output*))
  (let* ((thr (make-thread (lambda ()
                             (block eval-cmd
                               (handler-bind
                                   ((error (lambda (e)
                                             (format *error-output* "~a~%" e)
                                             #+sbcl
                                             (sb-debug:print-backtrace)
                                             (return-from eval-cmd))))
                                 (eval (read-from-string exp)))))
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
      (set-current-pgid pgid)
      (setf *current-job* job))
    (multiple-value-bind (pid status) (sb-posix:waitpid (car pids) sb-posix:wuntraced)
      (declare (ignore pid))
      (clsh.external-command:set-current-pgid *clsh-pgid*)
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

(defun parse-shell-to-lisp (shell-lst)
  (format nil "~w" shell-lst))

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

(defun create-job (cmds bg-flag)
  (multiple-value-bind (trans-cmds result)
      (check-command-executable cmds)
    (when result
      (let ((created-job nil))
        (mapc (lambda (cmd)
                (setf created-job
                      (case (first cmd)
                        (clsh.parser:lisp
                         (create-lisp-task (cadr cmd)))
                        (clsh.parser:shell
                         (create-command-task (cadr cmd))))
                      ))
              trans-cmds)
        (unless bg-flag
          (wait-job created-job))))))

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
