#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.util
  (:use :cl)
  (:import-from :cl-ppcre
                :regex-replace-all)
  (:import-from :alexandria
                :make-keyword))
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
(defun load-handler (server)
  "Loads a handler system in run-time. SERVER must be a symbol or a keyword.

Example:
  (load-handler :hunchentoot)"
  (let ((system
         (make-keyword (format nil "clack-handler-~(~A~)" server))))
    #+quicklisp (ql:quickload system :verbose nil)
    #-quicklisp (asdf:load-system system :verbose nil)))

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
Eitaro Fukamachi (e.arrows@gmail.com)
"
