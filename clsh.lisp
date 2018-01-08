(require :alexandria)
(require :cl-readline)
(require :cl-ppcre)

(load #P"jobs.lisp")

(defpackage clsh
  (:use    common-lisp
           alexandria
           clsh-jobs)
  (:export run))

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

                                        ;TODO パイプ処理については検討が必要
(defun run-program-wait (cmd args &key (input t))
  (let* ((os (make-string-output-stream))
         (job (create-job cmd args input os)))
    (wait-job job)
    (get-output-stream-string os)))
(defun run-program-no-wait (cmd args &key (input t))
  (create-job cmd args input :stream))

(defvar *command-standard-input* nil)

(set-macro-character #\] (get-macro-character #\)))
(set-dispatch-macro-character #\# #\[
                              (lambda (stream char1 char2)
                                (declare (ignore char1 char2))
                                (setf (readtable-case *readtable*) :preserve)
                                (unwind-protect
                                     (let ((command-line (read-delimited-list #\] stream t)))
                                       (list 'run-program-no-wait (princ-to-string (car command-line))
                                             `',(mapcar #'princ-to-string (rest command-line))
                                             ':input '*command-standard-input*))
                                  (setf (readtable-case *readtable*) :upcase))))

(defmacro rd (&rest body)
  (cons 'progn
        (append (mapcar (lambda (x)
                          `(setf *command-standard-input* ,x))
                        body)
                '((setf *command-standard-input* nil)))))

;;; Define and register function that does custom completion: if user enters
;;; first word, it will be completed as a verb, second and later words will
;;; be completed as fruits.
(defun complete-all-symbols (text start end)
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
    (select-completions (mapcar #'symbol-name (package-symbols-in-current)))))

(rl:register-function :complete #'complete-all-symbols)

(defvar *readline-name* "clsh")

                                        ;TODO should send adding request to cl-readline
                                        ;no entry in cl-readline
(defun add-history (text)
  (cffi:foreign-funcall "add_history" :string text :void))
(defun read-history ()
  (cffi:foreign-funcall "read_history" :string (namestring +history-file+) :int))
(defun write-history ()
  (cffi:foreign-funcall "write_history" :string (namestring +history-file+) :int))

                                        ;TODO using another name space might be better.
(defun find-command-symbol (cmds)
  (multiple-value-bind (sym statsu)
      (find-symbol (string-upcase (car cmds)) 'clsh-commands )
    (if (eq statsu :external)
        sym
        nil)))

(defun cmdline-execute (line)
  (multiple-value-bind (cmds-str bg-flg) (ppcre:regex-replace "\\s*&\\s*$" line "")
    (let* ((cmds (ppcre:split "[ 	]+" cmds-str))
           (func-sym (find-command-symbol cmds)))
      (if (fboundp func-sym)
          (apply func-sym (cdr cmds))
          (if bg-flg
              (run-program-no-wait (car cmds) (cdr cmds))
              (princ (run-program-wait (car cmds) (cdr cmds))))))))

(defun run ()
  (read-history)
  (do ((i 0 (1+ i))
       (text ""))
      (nil)
    (handler-case
        (progn
          (setf text
                (rl:readline :prompt (format nil "~a:[~a]> " (package-name *package*) i)
                             :add-history t
                             :novelty-check #'novelty-check))
          (cond ((or (ppcre:scan "^ 	*$" text) (= (length text) 0))) ;do nothing
                ((ppcre:scan "^[ 	]*\\(" text)
                 (eval (read-from-string text))
                 (fresh-line))
                (t
                 (cmdline-execute text)))
          (pick-finished-jobs))
      (error (c) (format *error-output* "~a~%" c))
      (sb-sys:interactive-interrupt (i) (format *error-output* "~a~%" i)))))

(load #p"commands.lisp")
