(defpackage clsh.commands
  (:use    common-lisp
           clsh
           clsh.jobs)
  (:export exit cd fg bg jobs))

(in-package clsh.commands)

(defvar *before-directory* nil)

#+sbcl
(defun exit ()
  (clsh::write-history)
  (sb-ext:exit))
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
