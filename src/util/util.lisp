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
                :ascii-string-to-byte-array
                :byte-array-to-hex-string
                :digest-sequence
                :make-digest
                :make-hmac
                :update-hmac
                :hmac-digest))
(in-package :clack.util)

(cl-syntax:use-syntax :annot)

(defun normalize-key (name)
  "Returns a keyword of NAME."
  (etypecase name
    (keyword name)
    ((or string symbol) (make-keyword name))))

@export
(defmacro getf* (place key)
  "Similar to `getf', but accepts a string, a keyword, or a symbol as KEY."
  `(getf ,place (normalize-key ,key)))

@export
(defmacro remf* (place key)
  "Similar to `remf', but accepts a string, a keyword, or a symbol as KEY."
  `(remf ,place (normalize-key ,key)))

@export
(defun nappend (&rest list-of-list)
  "Similar to `nconc', but assures LIST-OF-LIST to be overwritten with the result."
  (loop with res = (pop list-of-list)
        for list in list-of-list
        do (rplacd (last res) list)
        finally (return res)))

@export
(defun merge-plist (p1 p2)
  "Merges two plists into one plist.
If there are same keys in the two plists, the one in P2 is adopted.

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
  "Returns a handler package. SERVER must be a symbol or a keyword without \"Clack.Handler.\" prefix.

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
  "Loads a handler system in run-time. SERVER must be a symbol or a keyword.

Example:
  (load-handler :hunchentoot)"
  (let ((system
         (make-keyword (format nil "clack-handler-~(~A~)" server))))
    #+quicklisp (ql:quickload system :verbose nil)
    #-quicklisp (asdf:load-system system :verbose nil)))

@export
(defun hmac-sha1-hex-string (string secret)
  (let ((hmac (make-hmac (ascii-string-to-byte-array secret) :sha1)))
    (update-hmac hmac (ascii-string-to-byte-array string))
    (byte-array-to-hex-string (hmac-digest hmac))))

@export
(defun generate-random-id ()
  "Generates a random token."
  (byte-array-to-hex-string
   (digest-sequence
    (make-digest :SHA1)
    (ascii-string-to-byte-array
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
  "Applys a middleware to the `app'. This function is for resolving symbol packages in run-time."
  (funcall (intern (symbol-name :wrap)
                   (find-package :clack.middleware))
           (apply #'make-instance
                  (intern (symbol-name mw-class-name) mw-package)
                  args)
           app))

(doc:start)

@doc:NAME "
Clack.Util - Utilities for Clack core and middleware development.
"

@doc:DESCRIPTION "
Most of the time, Clack uses other utility libraries such as Alexandria, but I realized they were not sufficient for Clack.

See each description of these functions for the details.
"

@doc:AUTHOR "
Eitarow Fukamachi (e.arrows@gmail.com)
"
