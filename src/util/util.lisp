#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.util
  (:use :cl)
  (:import-from :cl-ppcre
                :regex-replace-all)
  (:import-from :ironclad
                :byte-array-to-hex-string
                :digest-sequence
                :make-digest)
  (:import-from :flexi-streams
                :string-to-octets))
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
(defmacro remf* (place key)
  "Similar to `remf` but allows many types for the `key', String,
Keyword or Symbol."
  `(remf ,place (normalize-key ,key)))

@export
(defun nappend (&rest list-of-list)
  "Similar to `nconc` but assures `list` to be rewritten with the result."
  (loop with res = (pop list-of-list)
        for list in list-of-list
        do (rplacd (last res) list)
        finally (return res)))

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

@export
(defun generate-random-id ()
  "Generate a random token."
  (byte-array-to-hex-string
   (digest-sequence
    (make-digest :SHA1)
    (flex:string-to-octets
     (format nil "~A~A"
      (random 1.0) (get-universal-time))))))

@export
(defun html-encode (str)
  (ppcre:regex-replace-all
   "([&><\"'])"
   str
   #'(lambda (match &rest regs)
       (declare (ignore regs))
       (cond
         ((string= "&" match) "&amp;")
         ((string= ">" match) "&gt;")
         ((string= "<" match) "&lt;")
         ((string= "\"" match) "&quot;")
         ((string= "'" match) "&#39;")))
   :simple-calls t))

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
