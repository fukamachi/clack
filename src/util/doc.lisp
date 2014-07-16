#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.util.doc
  (:nicknames :doc)
  (:use :cl
        :split-sequence))
(in-package :clack.util.doc)

(cl-syntax:use-syntax :annot)

@export
'#.(defvar *section-plist*
       '(:synopsis "SYNOPSIS"
         :explanation "EXPLANATION"
         :dependencies "DEPENDENCIES"
         :description "DESCRIPTION"
         :example "EXAMPLE"
         :see "SEE ALSO"
         :author "AUTHOR"
         :contributors "CONTRIBUTORS"
         :copyright "COPYRIGHT"
         :license "LICENSE"))

@export
(defun start ()
  "Clear the docstring of `*package*'.
I recommend you to put `(doc:start)' before calling doc functions,
because they append sections duplicately when the packaged is reloaded."
  (setf (documentation *package* t) ""))

@export
(defun doc (header &optional (string "") (level 1))
  "Set documentation to current package"
  (setf (documentation *package* t)
        (concatenate 'string
                     (documentation *package* t)
                     (section header string level))))

@export
(defun section (header &optional (string "") (level 1))
  (format nil "~:[~;~:*~V@{~A~:*~}~* ~A~2&~]~A~2&"
          level "#" header (string-left-trim #(#\Newline) string)))

@export
(defun name (string)
  (doc string))

#.`(progn
     ,@(loop for (fn-name sec) on *section-plist* by #'cddr
             collect
             `@export
               (defun ,(intern (symbol-name fn-name)) (string)
                 (doc ,sec
                      (string-left-trim #(#\Newline) string)
                      2))))

(doc::start)

@doc::NAME "
Clack.Util.Doc - For writing Clack documentations.
"

@doc::SYNOPSIS "

    ;; Clear documentation of `*package*'.
    (doc:start)
    
    (doc:NAME \"
    Clack - Web Application Environment for Common Lisp
    \")
    
    (doc:DESCRIPTION \"
    Clack is a Web Application Environment for Common Lisp inspired by Python's WSGI and Ruby's Rack. Your awesome framework should base on this.
    \")

    ;; I recommend to use with cl-annot (https://github.com/arielnetworks/cl-annot).
    ;; It allows you to write docs by annotation-style.
    (cl-annot:enable-annot-syntax)
    
    (doc:start)
    
    @doc:NAME \"
    Clack - Web Application Environment for Common Lisp
    \"
"

@doc::DESCRIPTION "
Clack.Util.Doc enables easy writing package documentations with Markdown.
"

@doc::AUTHOR "
* Eitaro Fukamachi (e.arrows@gmail.com)
"

@doc::SEE "
* [cl-annot](https://github.com/arielnetworks/cl-annot)
"
