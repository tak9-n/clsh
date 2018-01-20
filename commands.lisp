(defpackage clsh.commands
  (:use    common-lisp
           clsh
           clsh.jobs)
  (:export exit cd fg bg jobs))

(in-package clsh.commands)

#+sbcl
(defun exit ()
  (clsh::write-history)
  (sb-ext:exit))
#+sbcl
(defun cd (&optional dir)
  (sb-posix:chdir (if dir
                      (coerce dir 'simple-string)
                      (namestring (user-homedir-pathname)))))
(defun fg (&optional jobno)
  (clsh.jobs:make-job-active jobno t))
(defun bg (&optional jobno)
  (clsh.jobs:make-job-active jobno nil))
(defun jobs ()
  (clsh.jobs:show-jobs))
