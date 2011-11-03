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
                :regex-replace-all
                :split
                :quote-meta-chars)
  (:import-from :alexandria
                :make-keyword))
(in-package :clack.util.route)

(cl-annot:enable-annot-syntax)

@export
(defclass <url-rule> ()
     ((request-method :type symbol
                      :initarg :method
                      :initform :get
                      :accessor request-method)
      (url :type string
           :initarg :url
           :accessor url)
      (regex :type string
             :initarg :regex
             :accessor regex)
      (format-string :type string
                     :accessor format-string)
      (param-keys :type list
                  :accessor param-keys)))

(defclass <regex-url-rule> (<url-rule>) ())

@export
(defun make-url-rule (url &key (method :get) regexp)
  "Construct `<url-rule>` and return it. You must always use this function when you need `<url-rule>`."
  (if regexp
      (make-instance '<regex-url-rule> :method method :url url)
      (make-instance '<url-rule> :method method :url url)))

(defmethod initialize-instance :after ((this <url-rule>) &key)
  (compile-rule this))

(defmethod initialize-instance :after ((this <regex-url-rule>) &key)
  (setf (regex this) (url this)
        (format-string this)
        (ppcre:regex-replace-all "\\(.+?\\)" (regex this) "~A")))

(defmethod compile-rule ((this <url-rule>))
  (loop with list = (split "(?::([\\w-]+)|(\\*))" (url this)
                           :with-registers-p t :omit-unmatched-p t)
        for (prefix name) on list by #'cddr
        collect (ppcre:regex-replace-all
                 "[^\\?\\/\\w-]" prefix
                 #'escape-special-char
                 :simple-calls t) into re
        collect prefix into cs
        if (string= name "*")
          collect :splat into names
          and collect "(.*?)" into re
          and collect "~A" into cs
        else if name
          collect (make-keyword (string-upcase name)) into names
          and collect "([^/?#]+)" into re
          and collect "~A" into cs
        finally
     (setf (regex this) (format nil "^~{~A~}$" re)
           (format-string this) (format nil "~{~A~}" cs)
           (param-keys this) names)))

(defun escape-special-char (char)
  (let ((enc (clack.util.hunchentoot:url-encode (string char))))
    (cond
      ((string= char " ") (format nil "(?:~A|~A)" enc (escape-special-char #\+)))
      ((string= enc char) (ppcre:quote-meta-chars enc))
      (t enc))))

(defmethod match-method-p ((this <url-rule>) method)
  (or (string= :ANY (request-method this))
      (string= method (request-method this))))

@export
(defmethod match ((this <url-rule>) method url-string)
  "Check whether the `url-string` matches to `this`. This method is for `<url-rule>`.
Return two values, matched URL and Rule parameters as a plist.
If the url-rule is containing Wildcard rules, they will be collected as :splat.

Example:
    (match (make-url-rule \"/hello/:name\") :GET \"/hello/fukamachi\")
    ;=> \"/hello/fukamachi\"
        (:NAME \"fukamachi\")

    (match (make-url-rule \"/say/*/to/*\") :ANY \"/say/hello/to/world\")
    ;=> \"/say/hello/to/world\"
        (:SPLAT (\"hello\" \"world\"))
"
  @type string url-string
  (when (match-method-p this method)
    (multiple-value-bind (matchp values)
        (scan-to-strings (regex this) url-string)
      (when matchp
        (values matchp
                (loop for key in (param-keys this)
                      for val across values
                      if (eq key :splat)
                        collect val into splat
                      else
                        append (list key val) into result
                      finally
                   (return (if splat
                               `(:splat ,splat ,@result)
                               result))))))))

@export
(defmethod match ((this <regex-url-rule>) method url-string)
  "Check whether the `url-string` matches to `this`. This method is for `<regex-url-rule>`.
Return two values, matched URL and Rule parameters as a plist.
Captured strings in `url-string` are collected as :captures.

Example:
    (match (make-url-rule \"/hello([\\w]+)\" :regexp t)
           \"/hello/world\")
    ;=> \"/hello/world\"
        (:CAPTURES (\"world\"))
"
  (when (match-method-p this method)
    (multiple-value-bind (matchp values)
        (scan-to-strings (regex this) url-string)
      (when matchp
        (values matchp
                `(:captures ,(coerce values 'list)))))))

@export
(defmethod url-for ((this <url-rule>) params)
  "Return an URL from a rule and parameters.

Example:
    (url-for (make-url-rule \"/hello/:name\")
             '(:name \"fukamachi\"))
    ;=> \"/hello/fukamachi\"
"
  (values
   (apply #'format nil (format-string this)
          (loop for key in (param-keys this)
                if (eq key :splat)
                  collect (pop (getf params key))
                else
                  collect (getf params key)
                  and do (remf params key)))
   params))

@export
(defmethod url-for ((this <regex-url-rule>) params)
  "Return an URL from a rule and parameters.

Example:
    (url-for (make-url-rule \"/hello/:name\")
             '(:name \"fukamachi\"))
    ;=> \"/hello/fukamachi\"
"
  (values (apply #'format nil (format-string this)
                 (getf params :captures))
          (and (remf params :captures) params)))

(doc:start)

@doc:NAME "
Clack.Util.Route - Class for Sinatra-compatible URL rule.
"

@doc:SYNOPSIS "
    (defvar *url-rule* (make-url-rule \"/hello/:name\"))
    
    (match *url-rule* \"/hello/fukamachi\")
    ;=> (NAME \"fukamachi\")
    (match *url-rule* \"/bye/fukamachi\")
    ;=> NIL
    
    (url-for *url-rule* '(:name \"fukamachi\"))
    ;=> \"/hello/fukamachi\"
"

@doc:DESCRIPTION "
Clack.Util.Route provides a Sinatra-compatible routing class.

### Named Parameter

    (match (make-url-rule \"/hello/:name\") \"/hello/fukamachi\")
    ;=> \"/hello/fukamachi\"
        (:NAME \"fukamachi\")

### Wildcard Parameter

    (match (make-url-rule \"/say/*/to/*\") \"/say/hello/to/world\")
    ;=> \"/say/hello/to/world\"
        (:SPLAT (\"hello\" \"world\"))

### Optional Parameter

    (match (make-url-rule \"/?:foo?/?:bar?\") \"/hello/world\")
    ;=> \"/hello/world\"
        (:FOO \"hello\" :BAR \"world\")
    (match (make-url-rule \"/?:foo?/?:bar?\") \"/hello\")
    ;=> \"/hello\"
        (:FOO \"hello\" :BAR NIL)
    (match (make-url-rule \"/?:foo?/?:bar?\") \"/\")
    ;=> \"/\"
        (:FOO NIL :BAR NIL)

### Regular Expression

    (match (make-url-rule \"/hello([\\w]+)\" :regexp t)
           \"/hello/world\")
    ;=> \"/hello/world\"
        (:CAPTURES (\"world\"))
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
* Tomohiro Matsuyama (tomo@cx4a.org)

Note: `compile-rule` was originally written by Tomohiro Matsuyama as `parse-url-rule` in Clack.App.Route.
"
