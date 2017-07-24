(require :alexandria)
(require :cl-readline)
(require :cl-ppcre)

(cl:defpackage :clsh
  (:use    #:common-lisp
           #:alexandria)
  (:export #:run))

(in-package :clsh)

(defconstant +history-file+ (merge-pathnames (user-homedir-pathname) #p".clsh_history"))

;;; Let's also create a custom command and bind it to some key sequence so
;;; user can invoke it. In this example user can automagically insert phrase
;;; 'inserted text' pressing Control-o.

(defun print-some-text (arg key)
  (declare (ignore arg key))
  (format t "inserted text~%"))

(rl:bind-keyseq "\\C-o" #'print-some-text)

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

#+sbcl
(defun run-program-wait (cmd args)
  (let* ((os (make-string-output-stream))
         (proc (sb-ext:run-program cmd args :wait nil :search t :output os)))
    (sb-ext:process-wait proc t)
    (values (get-output-stream-string os) (sb-ext:process-exit-code proc))))
#+sbcl
(defun run-program-no-wait (cmd args)
  (sb-ext:run-program cmd args :wait nil :search t :output :stream))

(set-macro-character #\] (get-macro-character #\)))
(set-dispatch-macro-character #\# #\[
  (lambda (stream char1 char2)
    (declare (ignore char1 char2))
    (setf (readtable-case *readtable*) :preserve)
    (unwind-protect
         (let ((command-line (read-delimited-list #\] stream t)))
           (list 'run-program-wait (princ-to-string (car command-line))
                 `',(mapcar #'princ-to-string (rest command-line))))
      (setf (readtable-case *readtable*) :upcase))))

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

(defvar *clsh-history-path* #P"~/.clsh_history")
(defvar *readline-name* "clsh")

(defun add-history (text)
  (cffi:foreign-funcall "add_history" :string text :void))

(defun read-history ()
  (cffi:foreign-funcall "read_history" :string (namestring +history-file+) :int))
(defun write-history ()
  (cffi:foreign-funcall "write_history" :string (namestring +history-file+) :int))

(defun clsh-exit ()
  (write-history)
  (sb-ext:exit))

(defun run ()
  (read-history)
  (do ((i 0 (1+ i))
       (text ""))
      (nil)
    (setf text
          (rl:readline :prompt (format nil "~a:[~a]> " (package-name *package*) i)
                       :add-history t
                       :novelty-check #'novelty-check))
    (cond ((or (ppcre:scan "^ 	*$" text) (= (length text) 0))) ;do nothing
          ((ppcre:scan "^[ 	]*\\(" text)
           (format t "~s~%" (eval (read-from-string text))))
          (t
           (let ((cmds (ppcre:split "[ 	]+" text)))
             (princ (run-program-wait (car cmds) (cdr cmds))))))))
