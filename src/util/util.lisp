#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Util

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)

(defpackage clack.util
  (:use :cl)
  (:export :namespace
           :getf*
           :getf-all
           :merge-plist
           :enable-duck-reader))

(in-package :clack.util)

(defmacro namespace (name &rest body)
  `(progn
     (in-package :cl-user)
     (defpackage ,(symbol-name name) ,@body)
     (in-package ,(symbol-name name))))

(defun normalize-key (name)
  "key must be a keyword."
  (etypecase name
    (string (intern name :keyword))
    (keyword name)
    (symbol (intern (symbol-name name) :keyword))))

(defmacro getf* (place key)
  `(getf ,place (normalize-key ,key)))

(defun getf-all (plist key)
  "This is a version of `getf' enabled to manage multiple keys. If the `plist' has two or more pairs that they have given `key' as a key, returns the values of each pairs as one list."
  (loop with params = nil
        for (k v) on plist by #'cddr
        if (string= k key)
          do (push v params)
        finally (return (if (cdr params)
                            (nreverse params)
                            (car params)))))

(defun (setf getf-all) (val plist key)
  (declare (ignore val plist key))
  (error "TODO"))

(defun merge-plist (p1 p2)
  "Merge two plist into one plist."
  (loop with notfound = '#:notfound
        for (indicator value) on p1 by #'cddr
        when (eq (getf p2 indicator notfound) notfound) 
          do (progn
               (push value p2)
               (push indicator p2)))
  p2)

(defun duck-function (fn obj)
  (symbol-function (intern (symbol-name fn) (symbol-package (type-of obj)))))

(defmacro duckcall (fn obj &body body)
  `(funcall (duck-function ,fn ,obj) ,obj ,@body))

(defmacro duckapply (fn obj &body body)
  `(apply (duck-function ,fn ,obj) ,obj ,@body))

(defun duck-reader (stream arg)
  (declare (ignore arg))
  (let ((args (gensym "ARGS"))
        (fn (read-preserving-whitespace stream))
        (obj (read-preserving-whitespace stream)))
    `(lambda (&rest ,args) (duckapply ',fn ,obj ,args))))

(defun %enable-duck-reader ()
  (set-macro-character #\_ #'duck-reader)
  (values))

(defmacro enable-duck-reader ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%enable-duck-reader)))
