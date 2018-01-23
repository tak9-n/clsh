(require :alexandria)
(require :cl-readline)
(require :cl-ppcre)

(load #P"jobs.lisp")
(load #P"parser.lisp")

(defpackage clsh
  (:use    common-lisp
           alexandria
           clsh.jobs)
  (:export run *prompt-function*))

(in-package :clsh)

(defconstant +history-file+ (merge-pathnames (user-homedir-pathname) #p".clsh_history"))

;;; Let's write novelty-check, so if the actual line is equal to the most
;;; recent history line it will not be added to the history.

(defun novelty-check (x y)
  (string/= (string-trim " " x)
            (string-trim " " y)))

(defun package-symbols-in-current ()
  (let ((pkgs nil))
    (do-symbols (pkg *package* pkgs)
      (push pkg pkgs))))

(defun package-external-symbols (package)
  (let ((pkgs nil))
    (do-external-symbols
        (pkg package pkgs)
      (push pkg pkgs))))

(defvar *command-list* nil)
(defvar *command-hash* nil)

(defun lisp-syntax-p (text)
  (ppcre:scan "^[ 	]*\\(" text))

(defun command-path-specified-p (cmd)
  (ppcre:scan "/" cmd))

(defun command-with-path (cmd hash)
  (if (command-path-specified-p cmd)
      cmd
      (let ((cmd-path (gethash cmd hash nil)))
        (if cmd-path
            (namestring cmd-path)
            nil))))

#+sbcl
(defun executable-p (path)
  (handler-case
      (progn (sb-posix:access path sb-posix:x-ok)
             t)
    (sb-posix:syscall-error (e) (declare (ignore e)) nil)))

(defun run-program-no-wait (cmds &key (input 0) (output :stream))
  (declare (ignore input output)) ;TODO remove when implement command pipe.
  (create-job
   (mapcar (lambda (cmd-spec)
             (let* ((cmd (first cmd-spec))
                    (path-cmd (command-with-path cmd *command-hash*)))
               (if (and path-cmd (executable-p path-cmd))
                   (cons path-cmd (rest cmd-spec))
                   (progn
                     (format t "not found \"~a\" command~%" cmd)
                     (return-from run-program-no-wait nil)))))
           cmds)
   0 1))

(defun run-program-wait (cmds &key (input t))
  (let* ((os (make-string-output-stream))
         (job (run-program-no-wait cmds :input input :output os)))
    (when job
        (wait-job job)
        (get-output-stream-string os))))

(set-macro-character #\] (get-macro-character #\)))
(set-dispatch-macro-character #\# #\[
                              (lambda (stream char1 char2)
                                (declare (ignore char1 char2))
                                (setf (readtable-case *readtable*) :preserve)
                                (unwind-protect
                                     (let ((command-line (read-delimited-list #\] stream t)))
                                       (list 'run-program-no-wait (princ-to-string (car command-line))
                                             `',(mapcar #'princ-to-string (rest command-line))
                                             ':output :stream))
                                  (setf (readtable-case *readtable*) :upcase))))

;;; Define and register function that does custom completion: if user enters
;;; first word, it will be completed as a verb, second and later words will
;;; be completed as fruits.
(defun complete (comp-list text start end)
  (declare (ignore start end))
  (labels ((common-prefix (items)
             (subseq
              (car items) 0
              (position
               nil
               (mapcar
                (lambda (i)
                  (every (lambda (x)
                           (char= (char (car items) i)
                                  (char x           i)))
                         (cdr items)))
                (iota (reduce #'min (mapcar #'length items)))))))
           (select-completions (list)
             (let ((els (remove-if-not (curry #'starts-with-subseq text)
                                       list)))
               (if (cdr els)
                   (cons (common-prefix els) els)
                   els))))
    (select-completions comp-list)))

;notice: destrucvice!!
(defun sort-by-length (lst)
  (sort lst (lambda (x y) (< (length x) (length y)))))

(defun build-command-hash ()
  (let ((paths (mapcar (lambda (ps) (make-pathname :directory ps :name :wild))
                       (ppcre:split ":" (sb-posix:getenv "PATH"))))
        (command-list))
    (setf *command-hash* (make-hash-table :test #'equal))
    (mapc (lambda (p)
            (mapc (lambda (f)
                    (when (executable-p f)
                      (let ((name (pathname-name f)))
                        (setf (gethash name *command-hash*) f)
                        (push name command-list))))
                  (directory p)))
          paths)
    (setf *command-list* (sort-by-length command-list))))

(defun complete-cmdline (text start end)
  (complete
   (if (lisp-syntax-p rl:*line-buffer*)
       (sort-by-length (mapcar #'symbol-name (package-symbols-in-current)))
       *command-list*)
   text start end))

(rl:register-function :complete #'complete-cmdline)

(defvar *readline-name* "clsh")

                                        ;TODO should send adding request to cl-readline
                                        ;no entry in cl-readline
(defun add-history (text)
  (cffi:foreign-funcall "add_history" :string text :void))
(defun read-history ()
  (cffi:foreign-funcall "read_history" :string (namestring +history-file+) :int))
(defun write-history ()
  (cffi:foreign-funcall "write_history" :string (namestring +history-file+) :int))

(defun find-command-symbol (cmd)
  (multiple-value-bind (sym statsu)
      (find-symbol (string-upcase cmd) 'clsh.commands)
    (if (eq statsu :external)
        sym
        nil)))

(defun cmdline-execute (line)
  (let* ((p (clsh.parser:parse-cmdline line))
         (cmds (cdr p))
         (bg-flg (car p))
         (func-sym (find-command-symbol (caar cmds))))
    (if (fboundp func-sym)
        (progn
          (apply func-sym (cdar cmds))
          (fresh-line))
        (if bg-flg
            (run-program-no-wait cmds)
            (princ (run-program-wait cmds))))))

(defvar *prompt-function*
  (lambda (count)
    (declare (ignore count))
    (format nil "~a:[~a]> "
            (ppcre:regex-replace (concatenate 'string "^" (namestring (user-homedir-pathname)))
                                 (concatenate 'string (sb-posix:getcwd) "/") "~/")
            (package-name *package*))))

(defun run ()
  (read-history)
  (build-command-hash)
  (do ((i 0 (1+ i))
       (text ""))
      (nil)
    (handler-case
        (progn
          (setf text
                (rl:readline :prompt (funcall *prompt-function* i)
                             :add-history t
                             :novelty-check #'novelty-check))
          (cond ((or (ppcre:scan "^ 	*$" text) (= (length text) 0))) ;do nothing
                ((lisp-syntax-p text)
                 (princ (eval (read-from-string text)))
                 (fresh-line))
                (t
                 (cmdline-execute text)))
          (pick-finished-jobs))
      (error (c) (format *error-output* "~a~%" c))
      (sb-sys:interactive-interrupt (i) (format *error-output* "~a~%" i)))))

(load #p"commands.lisp")
