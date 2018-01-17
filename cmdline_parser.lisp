(require :maxpc)
(defpackage clsh-parser
  (:use common-lisp
        maxpc
        maxpc.char)
  (:export parse-cmdline))
(in-package clsh-parser)

(defun ?some-whitespace ()
  (%some (?whitespace)))
(defun ?any-whitespace ()
  (%any (?whitespace)))
(defun %delimiter ()
  (%or (?whitespace)
       (?newline)
       (?char #\|)))

(defun =words ()
  (=subseq
   (%some
    (%and (?not (%delimiter))
          (=element)))))


(defun =tilda-expansion ()
  (=destructure (_ user)
                (=list (?char #\~) (%maybe (=words)))
                (if user
                    (concatenate 'string "/home/" user)
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
  (=transform
   (%some
    (%or
     (=destructure (_ letter)
         (=list
          (?eq #\\)
          (=element))
       letter)
     (=words)))
   (lambda (x) (coerce x 'string))))

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
  (=destructure (cmd follow-cmd bg)
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
       (%maybe (%and (?any-whitespace) (?char #\&))))
    (cons bg (cons cmd follow-cmd))))

(defun parse-cmdline (source)
  (parse source (=command-line)))
