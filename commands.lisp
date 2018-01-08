(defpackage clsh-commands
  (:use    common-lisp
           clsh
           clsh-jobs)
  (:export exit cd fg bg jobs))

(in-package clsh-commands)

(defun exit ()
  (clsh::write-history)
  #+sbcl
  (sb-ext:exit))
(defun cd (dir)
  #+sbcl
  (sb-posix:chdir dir))
(defun fg (&optional jobno)
  (clsh-jobs:make-job-active jobno t))
(defun bg (&optional jobno)
  (clsh-jobs:make-job-active jobno nil))
(defun jobs ()
  (clsh-jobs:show-jobs))
