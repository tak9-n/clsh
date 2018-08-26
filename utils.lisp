(defpackage clsh.utils
  (:use
   common-lisp)
  (:export
   #:sort-by-length
   #:find-command-symbol))

(in-package clsh.utils)

;notice: destructive!!
(defun sort-by-length (lst)
  (sort lst (lambda (x y) (< (length x) (length y)))))

(defun find-command-symbol (cmd)
  (multiple-value-bind (sym status)
      (find-symbol (string-upcase cmd) 'clsh.commands)
    (if (eq status :external)
        sym
        nil)))
