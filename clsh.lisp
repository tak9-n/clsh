(require :alexandria)
(require :cl-readline)
(require :cl-ppcre)

(load #P"utils.lisp")
(load #P"parser.lisp")
(load #P"external_command.lisp")
(load #P"jobs.lisp")
(load #p"commands.lisp")

(defpackage clsh
  (:use    common-lisp
           alexandria
           clsh.jobs
           clsh.utils)
  (:export
   run
   *prompt-function*
  ))

(in-package clsh)

(defvar +history-file+)
(defvar *readline-name* "clsh")
(defvar *prompt-function*)

;;; Let's write novelty-check, so if the actual line is equal to the most
;;; recent history line it will not be added to the history.

(defun novelty-check (x y)
  (string/= (string-trim " " x)
            (string-trim " " y)))

(defun package-symbols (package)
  (let ((pkgs nil))
    (do-symbols (pkg package pkgs)
      (push pkg pkgs))))

(defun package-external-symbols (package)
  (let ((pkgs nil))
    (do-external-symbols
        (pkg package pkgs)
      (push pkg pkgs))))

(defun pass-prefix (text lst &key (ignore-case nil))
  (remove-if-not (lambda (target)
                   (starts-with-subseq text target :test (if ignore-case #'equalp #'equal)))
                 lst))

;;; Define and register function that does custom completion: if user enters
;;; first word, it will be completed as a verb, second and later words will
;;; be completed as fruits.
(defun common-prefix (items &key (ignore-case nil))
  (let ((compare-func (if ignore-case
                          #'char-equal
                          #'char=)))
    (subseq
     (car items) 0
     (position
      nil
      (mapcar
       (lambda (i)
         (every (lambda (x)
                  (funcall compare-func
                           (char (car items) i)
                           (char x           i)))
                (cdr items)))
       (iota (reduce #'min (mapcar #'length items))))))))

(defun complete-by-list (comp-list text start end &key (ignore-case nil))
  (declare (ignore start end))
  (labels ((select-completions (list)
             (let ((els (pass-prefix text list :ignore-case ignore-case)))
               (if (cdr els)
                   (cons (common-prefix els :ignore-case ignore-case) els)
                   els))))
    (select-completions comp-list)))

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
  (declare (ignore start end))
  (multiple-value-bind (orig-path abs-path comp-str)
      (described-path-to-abs text)
    (when orig-path
      (let ((cmp-lst (mapcar (lambda (x)
                               (ppcre:regex-replace (concatenate 'string "^" abs-path)
                                                  (namestring x)
                                                  orig-path))
                             (get-complete-list-filename comp-str))))
        (when cmp-lst
          (cons (common-prefix cmp-lst) cmp-lst))))))

(defun complete-list-for-command (text start end)
  (let ((p (clsh.parser:parse-command-string rl:*line-buffer*)))
    (if (< 1 (length (clsh.parser:task-token-task (first (nreverse p)))))
        (complete-list-filename text start end)
        (if (or (ppcre:scan "/" text) (and (not (null p))  (equal text "")))
            (complete-list-filename text start end)
            (complete-by-list clsh.external-command:*command-list* text start end)))))

(defun all-symbol-name-list-in-package (package &key has-package-name external)
  (mapcar (lambda (p) (concatenate 'string
                                   (when has-package-name (package-name package))
                                   (cond ((not has-package-name)
                                          "")
                                         (external
                                          ":")
                                         (t
                                          "::"))
                                   (symbol-name p)))
          (if external
              (package-external-symbols package)
              (package-symbols package))))

(defun all-package-name-list ()
  (reduce (lambda (accum p)
            (nconc
             (mapcar (lambda (pn)
                       (concatenate 'string pn ":"))
                     (cons (package-name p) (package-nicknames p)))
             accum))
          (list-all-packages)
          :initial-value nil))

(defun complete-list-for-lisp (text start end)
  (multiple-value-bind (s e sa ea)
      (ppcre:scan "([^:]+)(:{1,2})" text)
    (declare (ignore e))
    (if (null s)
        (let ((comp-lst (complete-by-list
                         (sort-by-length
                          (nconc (all-symbol-name-list-in-package *package*)
                                 (all-package-name-list)))
                         text start end :ignore-case t)))
          (if (and (not (rest comp-lst)) (ppcre:scan ":$" (first comp-lst)))
              (complete-list-for-lisp (first comp-lst) start end)
              comp-lst))
        (let ((package-name (subseq text (svref sa 0) (svref ea 0)))
              (all-symbol? (= (- (svref ea 1) (svref sa 1)) 1)))
          (complete-by-list
           (sort-by-length
            (let ((p (find-package (intern (string-upcase package-name) "KEYWORD"))))
              (when p
                (mapcar (lambda (x)
                          (ppcre:regex-replace (concatenate 'string "^" (package-name p) ":")
                                               x
                                               (concatenate 'string (string-upcase package-name) ":")))
                        (all-symbol-name-list-in-package p
                                                         :has-package-name t
                                                         :external all-symbol?)))))
           text start end :ignore-case t)))))

(defun complete-cmdline (text start end)
  (multiple-value-bind (task-token-lst match-p end-p)
      (clsh.parser:parse-command-string rl:*line-buffer*)
    (declare (ignore match-p))
    (let ((last-token (first (nreverse task-token-lst))))
      (if (or (not end-p)
              (eq (first (clsh.parser:task-token-task last-token)) 'clsh.parser:lisp))
          (complete-list-for-lisp text start end)
          (complete-list-for-command text start end)))))


                                        ;TODO should send adding request to cl-readline
                                        ;no entry in cl-readline
(defun add-history (text)
  (cffi:foreign-funcall "add_history" :string text :void))
(defun read-history ()
  (cffi:foreign-funcall "read_history" :string (namestring +history-file+) :int))
(defun write-history ()
  (cffi:foreign-funcall "write_history" :string (namestring +history-file+) :int))

(defun run ()
  (setf +history-file+ (merge-pathnames (user-homedir-pathname) #p".clsh_history")
        *prompt-function* (lambda (count)
                            (declare (ignore count))
                            (format nil "~a:[~a]> "
                                    (ppcre:regex-replace (concatenate 'string "^" (namestring (user-homedir-pathname)))
                                                         (concatenate 'string (sb-posix:getcwd) "/") "~/")
                                    (package-name *package*))))
  (rl:register-function :complete #'complete-cmdline)

  (push (lambda () (write-history)) clsh.commands:*exit-hook*)

  (clsh.jobs:jobs-init)
  (clsh.parser:parser-init)

  (let ((rcfile (merge-pathnames (user-homedir-pathname) #P".clshrc")))
    (when (probe-file rcfile)
      (load rcfile)))

  (read-history)
  (clsh.external-command:build-command-hash)

  (do ((i 0 (1+ i))
       (text ""))
      (nil)
    (handler-bind
        ((sb-sys:interactive-interrupt (lambda (i)
                                         (format *error-output* "~%~a~%" i)
                                         (go cmd-loop))))
      (setf text
        (rl:readline :prompt (funcall *prompt-function* i)
                     :add-history t
                     :novelty-check #'novelty-check))
      (unless (ppcre:scan "^ *$" text)
        (multiple-value-bind (result match-p end-p)
            (clsh.parser:parse-cmdline text)
          (cond ((and match-p end-p)
                 (clsh.jobs:create-job (cdr result) (car result)))
                (match-p
                 (format *error-output* "incomplete command.~%") ;not support new line in the middle of command.
                 )
                (t
                 (format *error-output* "command parse error.~%")
                 ))))
      (pick-finished-jobs))
   cmd-loop))

(in-package common-lisp-user)
(defun v (x) x)
(export 'v)
