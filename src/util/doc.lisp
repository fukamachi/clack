#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.util.doc
  (:nicknames :doc)
  (:use :cl))
(in-package :clack.util.doc)

(cl-annot:enable-annot-syntax)

@export
(defun doc (header string &optional overwritep)
  "Set documentation to current package"
  (let ((docstring (format nil "~:[~;~:*# ~A~2%~]~A~%" header string)))
    (setf (documentation *package* t)
          (if overwritep
              docstring
              (concatenate 'string
                           (or (documentation *package* t) "")
                           docstring)))))

@export
(defun start ()
  "Clear the docstring of `*package'.
I recommend you to put `(doc:start)' before calling doc functions,
because they append sections duplicately when the packaged is reloaded."
  (setf (documentation *package* t) ""))

#.`(progn
     ,@(loop for fn in '(name
                         synopsis
                         explanation
                         dependencies
                         description
                         example
                         see
                         author
                         contributor
                         copyright)
             with string = (gensym "STRING")
             collect
             `@export
               (defun ,fn (,string) (doc ,(symbol-name fn)
                                         (string-left-trim #(#\Newline) ,string)))))

(doc:start)

@doc::NAME "
Clack.Util.Doc - For writing Clack documentations.
"

@doc::SYNOPSIS "

    ;; Flush documentation of `*package'.
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
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc::SEE "
* [cl-annot](https://github.com/arielnetworks/cl-annot)
"
