(defpackage clsh.commands
  (:use    common-lisp
           clsh.jobs)
  (:export exit cd fg bg jobs rehash *exit-hook*))

(in-package clsh.commands)

(defvar *before-directory* nil)
(defvar *exit-hook* nil)

#+sbcl
(defun exit ()
  (mapc (lambda (x) (funcall x)) *exit-hook*)
  (sb-posix:exit 0))

#+sbcl
(defun cd (&optional dir)
  (let ((dir-to-move (if (equal dir "-")
                         (if *before-directory*
                             *before-directory*
                             (namestring (user-homedir-pathname)))
                         (if dir
                             (coerce dir 'simple-string)
                             (namestring (user-homedir-pathname))))))
    (setf *before-directory* (sb-posix:getcwd))
    (sb-posix:chdir dir-to-move)
    (setf *default-pathname-defaults* (pathname (concatenate 'string (sb-posix:getcwd) "/")))))

(defun fg (&optional jobno)
  (clsh.jobs:make-job-active jobno t))
(defun bg (&optional jobno)
  (clsh.jobs:make-job-active jobno nil))
(defun jobs ()
  (clsh.jobs:show-jobs))
(defun rehash ()
  (clsh.external-command:build-command-hash)
  nil)
