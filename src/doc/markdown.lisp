(in-package :cl-user)
(defpackage clack.doc.markdown
  (:use :cl
        :cl-ppcre))
(in-package :clack.doc.markdown)

(cl-annot:enable-annot-syntax)

@export
(defun markdown-escape-string (string)
  (ppcre:regex-replace-all "([\\`*_{}\\[\\]()#.!<>])" string
   (lambda (stream match)
     @ignore stream
     (cond
       ((string= match "<") "&lt;")
       ((string= match ">") "&gt;")
       (t (concatenate 'string "\\" match))))
   :simple-calls t))

@export
(defun markdown-escape (stream object &optional colon-p at-sign-p)
  @ignore (colon-p at-sign-p)
  (format stream (markdown-escape-string (princ-to-string object))))
