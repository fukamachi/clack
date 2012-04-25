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
  (:import-from :alexandria
                :make-keyword)
  (:import-from :ironclad
                :byte-array-to-hex-string
                :digest-sequence
                :make-digest)
  (:import-from :flexi-streams
                :string-to-octets))
(in-package :clack.util)

(cl-syntax:use-syntax :annot)

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
    (keyword name)
    ((or string symbol) (make-keyword name))))

@export
(defmacro getf* (place key)
  "Similar to `getf' but allows many types for the `key', String, Keyword or Symbol."
  `(getf ,place (normalize-key ,key)))

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
(defun find-handler (server &key (force t))
  "Return a handler package. `server` must be a symbol or a keyword, not containing \"Clack.Handler.\" as a prefix.

Example:
  (find-handler :hunchentoot)"
  (let* ((handler-name (concatenate 'string
                                    "CLACK.HANDLER."
                                    (symbol-name server)))
         (pkg (find-package handler-name)))
    (when (and (not pkg) force)
      (load-handler server)
      (setf pkg (find-handler server :force nil)))
    (or pkg
        (error "Handler package is not found. Forgot to load it?: ~A"
               server))))

@export
(defun load-handler (server)
  "Load a handler system in run-time. `server` must be a symbol or a keyword.

Example:
  (load-handler :hunchentoot)"
  (let ((system
         (make-keyword (format nil "clack-handler-~(~A~)" server))))
    #+quicklisp (ql:quickload system :verbose nil)
    #-quicklisp (asdf:load-system system :verbose nil)))

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

@export
(defvar *tmp-directory*
  #+(or :win32 :mswindows) "c:\\hunchentoot-temp\\"
  #-(or :win32 :mswindows) "/tmp/hunchentoot/"
  "Directory for temporary files created by MAKE-TMP-FILE-NAME.")

(let ((counter 0))
  (declare (ignorable counter))
  @export
  (defun make-tmp-file-name (&optional (prefix "clack"))
    "Generates a unique name for a temporary file."
    (let ((tmp-file-name
           #+:allegro
           (pathname (system:make-temp-file-name prefix *tmp-directory*))
           #-:allegro
           (loop for pathname = (make-pathname :name (format nil "~A-~A"
                                                             prefix (incf counter))
                                               :type nil
                                               :defaults *tmp-directory*)
                 unless (probe-file pathname)
                 return pathname)))
      tmp-file-name)))

@export
(defun apply-middleware (app mw-class-name mw-package &rest args)
  "Apply a middleware to the `app'. This function is for resolving symbol packages in run-time."
  (funcall (intern (symbol-name :wrap)
                   (find-package :clack.middleware))
           (apply #'make-instance
                  (intern (symbol-name mw-class-name) mw-package)
                  args)
           app))

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
