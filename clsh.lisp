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
      (progn (sb-posix:access (namestring path) sb-posix:x-ok)
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

(defun pass-prefix (text lst)
  (remove-if-not (curry #'starts-with-subseq text) lst))

;;; Define and register function that does custom completion: if user enters
;;; first word, it will be completed as a verb, second and later words will
;;; be completed as fruits.
(defun common-prefix (items)
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

(defun complete (comp-list text start end)
  (declare (ignore start end))
  (labels ((select-completions (list)
             (let ((els (pass-prefix text list)))
               (if (cdr els)
                   (cons (common-prefix els) els)
                   els))))
    (select-completions comp-list)))

;notice: destructive!!
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
                  (directory p :resolve-symlinks nil)))
          paths)
    (setf *command-list* (sort-by-length command-list))))

(defun abs-path-specified-p (name)
  (ppcre:scan "^/" name))

(defun get-complete-list-filename (text)
  (let ((p (remove-if-not (lambda (x) (starts-with-subseq text (namestring x)))
                          (directory (make-pathname :name :wild :type :wild
                                                    :directory (pathname-directory (pathname text)))
                                     :resolve-symlinks nil))))
    (if (or (null p) (rest p) (pathname-name (first p)))
        p
        (get-complete-list-filename (namestring (first p))))))

(defun described-path-to-abs (path)
  (handler-case
      (let* ((orig-path (ppcre:regex-replace "([^/]*)$" path ""))
             (abs-pathname (truename orig-path))
             (dir-string (namestring abs-pathname)))
        (values orig-path
                dir-string
                (concatenate 'string dir-string (ppcre:scan-to-strings "([^/]+)$" path))))
    (sb-int:simple-file-error ()
      nil)))

(defun complete-list-filename (text start end)
  (multiple-value-bind (orig-path abs-path comp-str)
      (described-path-to-abs text)
    (when orig-path
      (let ((cmp-lst (mapcar (lambda (x)
                               (ppcre:regex-replace (concatenate 'string "^" abs-path)
                                                  (namestring x)
                                                  orig-path))
                             (get-complete-list-filename comp-str))))
        (cons (common-prefix cmp-lst) cmp-lst)))))

(defun complete-list-for-command (text start end)
  (let ((p (clsh.parser:parse-command-string rl:*line-buffer*)))
    (if (< 1 (length (first (nreverse p))))
        (complete-list-filename text start end)
        (if (ppcre:scan "/" text)
            (complete-list-filename text start end)
            (complete *command-list* text start end)))))

(defun complete-cmdline (text start end)
  (if (lisp-syntax-p rl:*line-buffer*)
      (complete
       (sort-by-length (mapcar #'symbol-name (package-symbols-in-current)))
       text start end)
      (complete-list-for-command text start end)))

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
    (handler-bind
        ((error (lambda (e)
                  (format *error-output* "~a~%" e)
                   #+sbcl
                   (sb-debug:print-backtrace)
                   (go cmd-loop)))
         (sb-sys:interactive-interrupt (lambda (i)
                                         (format *error-output* "~%~a~%" i)
                                         (go cmd-loop)
                                         )))
      (progn
        (setf text
              (rl:readline :prompt (funcall *prompt-function* i)
                           :add-history t
                           :novelty-check #'novelty-check))
        (cond ((or (ppcre:scan "^ 	*$" text) (= (length text) 0))) ;do nothing
              ((lisp-syntax-p text)
               (print (eval (read-from-string text)))
               (fresh-line))
              (t
               (cmdline-execute text)))
        (pick-finished-jobs)))
    cmd-loop
    ))

(load #p"commands.lisp")
