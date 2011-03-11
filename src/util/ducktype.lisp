#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.util.ducktype
  (:use :cl))
(in-package :clack.util.ducktype)

(cl-annot:enable-annot-syntax)

(defvar *previous-readtables* nil
  "Stack of readtables for duck-typing reader.")

@export
(defun duck-function (fn obj)
  "Detect correct function for `obj'.
The function should be interned a package which exports a class of `obj'."
  (symbol-function
   (intern (symbol-name fn)
           (symbol-package (type-of obj)))))

@export
(defmacro duckcall (fn obj &body body)
  "Call `duck-function' of the `obj'.
I supposed `enable-duck-reader' may help you."
  `(funcall (duck-function ,fn ,obj) ,obj ,@body))

@export
(defmacro duckapply (fn obj &body body)
  "Apply `duck-function' of the `obj'.
I supposed `enable-duck-reader' may help you."
  `(apply (duck-function ,fn ,obj) ,obj ,@body))

(defun duck-reader (stream arg)
  @ignore arg
  (let ((args (gensym "ARGS"))
        (fn (read-preserving-whitespace stream))
        (obj (read-preserving-whitespace stream)))
    `(lambda (&rest ,args) (duckapply ',fn ,obj ,args))))

(defun %enable-duck-reader ()
  (push *readtable* *previous-readtables*)
  (setq *readtable* (copy-readtable))
  (set-macro-character #\& #'duck-reader)
  (values))

(defun %disable-duck-reader ()
  (setq *readtable*
        (if *previous-readtables*
            (pop *previous-readtables*)
            (copy-readtable nil))))

@export
(defmacro enable-duck-reader ()
  "Enable duck-typing-reader.

Example:
  (&call app req)
"
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%enable-duck-reader)))

@export
(defmacro disable-duck-reader ()
  "Disable duck-typing-reader if it is enabled."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%disable-duck-reader)))

(doc:start)

@doc:NAME "
Clack.Util.Ducktype - Duck-typing with Common Lisp.
"

@doc:SYNOPSIS "
    ;; hoge
    (defpackage hoge
      (:use :cl)
      (:export :<hoge>))
    (in-package :hoge)
    (defclass <hoge> () ())
    (defmethod call ((this <hoge>)))
    
    ;; fuga
    (defpackage fuga
      (:use :cl)
      (:export :<fuga>))
    (in-package :fuga)
    (defclass <fuga> () ())
    (defmethod call ((this <fuga>)))
    
    ;; main
    (defpackage main
      (:use :cl
            :hoge
            :fuga
            :clack.util.ducktype))
    (in-package :main)
    
    (enable-duck-reader)
    
    (&call (make-instance '<hoge>))
    (&call (make-instance '<fuga>))
    
    (disable-duck-reader)
"

@doc:DESCRIPTION "
Clack.Util.Ducktype provides a way to call different objects which have a same name method, like duck-typing. These objects are no need to inherit each other.

This reader macro seems useful for Clack development, but, isn't used in Core now.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
