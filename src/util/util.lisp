#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
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
  "Similar to `defpackage', but the difference is ensure to be in :CL-USER before and to be in the new package after.
This may be useful for 'one-package-per-one-file' style."
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
  "Similar to `getf' but allows many types for the `key', String, Keyword or Symbol."
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
  "Merge two plist into one plist.
If same keys in two plist, second one will be adopted.

Example:
  (merge-plist '(:apple 1 :grape 2) '(:banana 3 :apple 4))
  ;;=> (:GRAPE 2 :BANANA 3 :APPLE 4)
"
  (loop with notfound = '#:notfound
        for (indicator value) on p1 by #'cddr
        when (eq (getf p2 indicator notfound) notfound)
          do (progn
               (push value p2)
               (push indicator p2)))
  p2)

@export
(defun find-handler (server)
  "Return a handler package. `server' must be a symbol or a keyword, not containing \"Clack.Handler.\" as a prefix.

Example:
  (find-handler :hunchentoot)"
  (let ((handler-name (concatenate 'string
                                    "CLACK.HANDLER."
                                    (symbol-name server))))
    (or (find-package handler-name)
        (error "Handler package is not found. Forgot to load it?: ~A"
               handler-name))))

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

(doc:start)

@doc:NAME "
Clack.Util - Utilities for Clack core or middleware development.
"

@doc:DESCRIPTION "
Most of time, Clack uses other utility libraries (ex. Alexandria), but I realized they were not enough for Clack.

See each description of these functions for detail.
"

@doc:AUTHOR "
Eitarow Fukamachi (e.arrows@gmail.com)
"
