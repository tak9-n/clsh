(defpackage clsh-commands
  (:use    common-lisp
           clsh
           clsh-jobs)
  (:export exit cd fg bg))

(in-package clsh-commands)

(defun exit ()
  (clsh::write-history)
  #+sbcl
  (sb-ext:exit))
(defun cd (dir)
  #+sbcl
  (sb-posix:chdir dir))
(defun fg (&optional job)
  (clsh-jobs:make-job-active (if job job (clsh-jobs:get-first-job)) t))
(defun bg (&optional job)
  (clsh-jobs:make-job-active (if job job (clsh-jobs:get-first-job)) nil))
