#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.util.route
  (:use :cl)
  (:import-from :cl-ppcre
                :scan-to-strings
                :split))
(in-package :clack.util.route)

(cl-annot:enable-annot-syntax)

(defclass <url-rule> ()
     ((url :type string
           :initarg :url
           :accessor url)
      (regex :type string
             :accessor regex)
      (format-string :type string
                     :accessor format-string)
      (param-keys :type list
                  :accessor param-keys)))

@export
(defun make-url-rule (url)
  (make-instance '<url-rule> :url url))

(defmethod initialize-instance :after ((this <url-rule>) &key)
  (compile-rule this))

(defmethod compile-rule ((this <url-rule>))
  (loop with list = (split ":([\\w-]+)" (url this) :with-registers-p t)
        while list
        for prefix = (pop list)
        for name = (pop list)
        collect prefix into re
        collect prefix into cs
        if name
          collect (intern (string-upcase name) :keyword) into names
          and collect "(.+?)" into re
          and collect "~A" into cs
        finally
     (setf (regex this) (format nil "^~{~A~}$" re)
           (format-string this) (format nil "~{~A~}" cs)
           (param-keys this) names)))

@export
(defmethod match ((this <url-rule>) url-string)
  @type string url-string
  (multiple-value-bind (matchp values)
      (scan-to-strings (regex this) url-string)
    (values matchp
            (loop for key in (param-keys this)
                  for val across values
                  append (list key val)))))

@export
(defmethod link-to ((this <url-rule>) params)
  @type list params
  (apply #'format nil (format-string this)
         (loop for key in (param-keys this)
               collect (getf params key))))

(doc:start)

@doc:NAME "
Clack.Util.Route - Class for Sinatra-like URL rule.
"

@doc:SYNOPSIS "
    (defvar *url-rule* (make-url-rule \"/hello/:name\"))
    
    (match *url-rule* \"/hello/fukamachi\")
    ;=> (NAME \"fukamachi\")
    (match *url-rule* \"/bye/fukamachi\")
    ;=> NIL
    
    (link-to *url-rule* '(:name \"fukamachi\"))
    ;=> \"/hello/fukamachi\"
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
* Tomohiro Matsuyama (tomo@cx4a.org)

Note: `compile-rule` was originally written by Tomohiro Matsuyama as `parse-url-rule` in Clack.App.Route.
"
