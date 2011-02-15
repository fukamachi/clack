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
  (:import-from :local-time
                :format-timestring
                :unix-to-timestamp
                :+rfc-1123-format+
                :+gmt-zone+))
(in-package :clack.util)

(cl-annot:enable-annot-syntax)

@export
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

@export
(defmacro getf* (place key)
  `(getf ,place (normalize-key ,key)))

@export
(defun getf-all (plist key)
  "This is a version of `getf' enabled to manage multiple keys. If the `plist' has two or more pairs that they have given `key' as a key, returns the values of each pairs as one list."
  (loop with params = nil
        for (k v) on plist by #'cddr
        if (string= k key)
          do (push v params)
        finally (return (if (cdr params)
                            (nreverse params)
                            (car params)))))

@export
(defun (setf getf-all) (val plist key)
  @ignore (val plist key)
  (error "TODO"))

@export
(defun merge-plist (p1 p2)
  "Merge two plist into one plist."
  (loop with notfound = '#:notfound
        for (indicator value) on p1 by #'cddr
        when (eq (getf p2 indicator notfound) notfound) 
          do (progn
               (push value p2)
               (push indicator p2)))
  p2)

;; Duck Typing

(defvar *previous-readtables* nil)

@export
(defun duck-function (fn obj)
  (symbol-function (intern (symbol-name fn) (symbol-package (type-of obj)))))

@export
(defmacro duckcall (fn obj &body body)
  `(funcall (duck-function ,fn ,obj) ,obj ,@body))

@export
(defmacro duckapply (fn obj &body body)
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
  (set-macro-character #\@ #'duck-reader)
  (values))

(defun %disable-duck-reader ()
  (setq *readtable*
        (if *previous-readtables*
            (pop *previous-readtables*)
            (copy-readtable nil))))

@export
(defmacro enable-duck-reader ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%enable-duck-reader)))

@export
(defmacro disable-duck-reader ()
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (%disable-duck-reader)))

;; LOCAL-TIME

@export
(defun now ()
  "Returns a timestamp representing the present moment."
  (multiple-value-bind (sec nsec)
      (values (- (get-universal-time)
                 #.(encode-universal-time 0 0 0 1 1 1970 0))
              0)
    (assert (and sec nsec) () "Failed to get the current time from the operating system. How did this happen?")
    (unix-to-timestamp sec :nsec nsec)))

@export
(defun format-rfc1123-timestring (destination timestamp)
  (format-timestring destination timestamp
                     :format +rfc-1123-format+
                     :timezone +gmt-zone+))
