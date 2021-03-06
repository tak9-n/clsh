(ql:quickload :maxpc)
(ql:quickload :cl-fad)

(defpackage clsh.parser
  (:use common-lisp
        cl-fad
        maxpc
        maxpc.char
        maxpc.input)
  (:export parse-cmdline
           parser-init
           parse-command-string
           lisp
           shell
           task-token-kind
           task-token-position
           task-token-task))
(in-package clsh.parser)

(defstruct task-token
  kind
  position
  task)

(defun ?some-whitespace ()
  (%some (?whitespace)))
(defun ?any-whitespace ()
  (%any (?whitespace)))
(defun %delimiter ()
  (%or (?whitespace)
       (?newline)
       (?char #\|)
       (?char #\&)
       (?char #\()))

(defun =words ()
  (=subseq
   (%some
    (?not (%delimiter)))))

(defun =single-quoted-string ()
  (=destructure (_ str _)
                (=list
                 (?char #\')
                 (=subseq (%any (?not (?char #\'))))
                 (?char #\'))
                str))

(defun =double-quoted-string ()
  (=destructure (_ str _)
                (=list
                 (?char #\")
                 (=subseq (%any (?not (?char #\"))))
                 (?char #\"))
                str))

(defun =strings-with-globing ()
  (=transform
   (%some
    (%or
     (=destructure (_ letter)
         (=list
          (?eq #\\)
          (=element))
         letter)
     (=words)))
   (lambda (x)
     (handler-case
         (let ((globbed-path (directory (car x))))
           (if globbed-path
               (mapcar #'namestring globbed-path)
               x))
       (error (c)
         (declare (ignore c))
         x)))))

(defun =string ()
  (%or
   (=single-quoted-string)
   (=double-quoted-string)))

(defun =command-as-shell ()
  (lambda (input)
    (funcall 
     (=destructure (cmd args)
                   (=list
                    (=words)
                    (%any
                     (=destructure (_ arg)
                                   (=list
                                    (?some-whitespace)
                                    (%or 
                                     (=string)
                                     (=strings-with-globing)))
                                   arg)))
                   (make-task-token :kind 'shell :position (input-position input) :task (cons cmd (alexandria:flatten args))))
    input)))

(defun =lisp-expression ()
  (=subseq
   (=list
    (?char #\()
    (?any-whitespace)
    (%some
     (%or
      '=lisp-expression/parser
      (=subseq (%some (?not (%or (?char #\() (?char #\))))))))
    (?any-whitespace)
    (?char #\))
    )))

(defun parser-init ()
 (setf (fdefinition '=lisp-expression/parser) (=lisp-expression)))

(defun =command-as-lisp ()
  (lambda (input)
    (funcall
     (=transform (=lisp-expression)
                 (lambda (exp)
                   (make-task-token :kind 'lisp :position (input-position input) :task exp)))
     input)))

(defun =command ()
  (%or (=command-as-lisp)
       (=command-as-shell)))

(defun =command-line ()
  (=destructure (cmd follow-cmd _ bg)
      (=list
       (=command)
       (%any
        (=destructure (_ cmd)
            (=list
             (=list (?any-whitespace)
                    (?char #\|)
                    (?any-whitespace))
             (=command))
          cmd))
       (?any-whitespace)
       (%maybe (=transform (?char #\&) (lambda (x) (declare (ignore x)) t))))
    (cons bg (cons cmd follow-cmd))))

(defun =command-string ()
  (=destructure (cmd follow-cmd _)
      (=list
       (=command)
       (%any
        (=destructure (_ cmd)
            (=list
             (=list (?any-whitespace)
                    (?char #\|)
                    (?any-whitespace))
             (=command))
          cmd))
       (?any-whitespace))
    (cons cmd follow-cmd)))

(defun parse-cmdline (source)
  (parse source (=command-line)))
(defun parse-command-string (source)
  (parse source (=command-string)))
