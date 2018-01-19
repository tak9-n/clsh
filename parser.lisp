(require :maxpc)
(require :cl-fad)

(defpackage clsh.parser
  (:use common-lisp
        cl-fad
        maxpc
        maxpc.char)
  (:export parse-cmdline))
(in-package clsh.parser)

(defun ?some-whitespace ()
  (%some (?whitespace)))
(defun ?any-whitespace ()
  (%any (?whitespace)))
(defun %delimiter ()
  (%or (?whitespace)
       (?newline)
       (?char #\|)
       (?char #\&)))

(defun =words ()
  (=subseq
   (%some
    (?not (%delimiter)))))


(defun =tilda-expansion ()
  (=destructure (_ user)
                (=list (?char #\~) (%maybe (%and (?not (?eq #\/)) (=words))))
                (if user
                    (namestring
                     (merge-pathnames-as-directory (pathname-parent-directory (user-homedir-pathname))
                                                   (make-pathname :directory (list :relative user))))
                    (namestring (user-homedir-pathname)))))

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

(defun =string-with-backslash ()
  (=subseq
   (%some
    (%or
     (=destructure (_ letter)
         (=list
          (?eq #\\)
          (=element))
       letter)
     (=words)))))

(defun =string ()
  (%or
   (=single-quoted-string)
   (=double-quoted-string)
   (=tilda-expansion)
   (=string-with-backslash)))

(defun =command ()
  (=destructure (cmd args)
      (=list
       (=words)
       (%any
        (=destructure (_ cmd)
            (=list
             (?some-whitespace)
             (=string)))))
    (cons cmd args)))

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

(defun parse-cmdline (source)
  (parse source (=command-line)))
