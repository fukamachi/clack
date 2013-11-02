#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.util.route
  (:use :cl
        :trivial-types)
  (:import-from :cl-ppcre
                :scan-to-strings
                :regex-replace-all
                :split
                :quote-meta-chars))
(in-package :clack.util.route)

(cl-syntax:use-syntax :annot)

@export
(defclass <url-rule> ()
  ((request-method :type (or symbol
                             (proper-list symbol))
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
          collect (read-from-string (concatenate 'string ":" name)) into names
          and collect "([^/?]+)" into re
          and collect "~A" into cs
        finally
     (setf (regex this) (format nil "^~{~A~}$" re)
           (format-string this) (ppcre:regex-replace-all "~A\\?" (format nil "~{~A~}" cs)
                                                         "~:[~;~:*~A~]")
           (param-keys this) names)))

(defun escape-special-char (char)
  (let ((enc (clack.util.hunchentoot:url-encode (string char))))
    (cond
      ((string= char " ") (format nil "(?:~A|~A)" enc (escape-special-char #\+)))
      ((string= enc char) (ppcre:quote-meta-chars enc))
      (t enc))))

(defmethod match-method-p ((this <url-rule>) method &key allow-head)
  (flet ((method-equal (rule-method)
           (or (string= :ANY rule-method)
               (string= method rule-method)
               (and (eq method :head)
                    allow-head
                    (string= :get rule-method)))))
    (typecase (request-method this)
      (list (some #'method-equal (request-method this)))
      (T (method-equal (request-method this))))))

@export
(defmethod match ((this <url-rule>) method url-string &key allow-head)
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
  (when (match-method-p this method :allow-head allow-head)
    (multiple-value-bind (matchp values)
        (scan-to-strings (regex this) url-string)
      (when matchp
        (values matchp
                (loop for key in (param-keys this)
                      for val across values
                      if (eq key :splat)
                        collect val into splat
                      else if val
                        append (list key val) into result
                      finally
                   (return (if splat
                               `(:splat ,splat ,@result)
                               result))))))))

@export
(defmethod match ((this <regex-url-rule>) method url-string &key allow-head)
  "Check whether the `url-string` matches to `this`. This method is for `<regex-url-rule>`.
Return two values, matched URL and Rule parameters as a plist.
Captured strings in `url-string` are collected as :captures.

Example:
    (match (make-url-rule \"/hello/([\\w]+)\" :regexp t)
           :GET \"/hello/world\")
    ;=> \"/hello/world\"
        (:CAPTURES (\"world\"))
"
  (when (match-method-p this method :allow-head allow-head)
    (multiple-value-bind (matchp values)
        (scan-to-strings (regex this) url-string)
      (when matchp
        (values matchp
                `(:captures ,(coerce values 'list)))))))

@export
(defmethod url-for ((url-rule <url-rule>) params)
  "Return an URL from a rule and parameters.

Example:
    (url-for (make-url-rule \"/hello/:name\")
             '(:name \"fukamachi\"))
    ;=> \"/hello/fukamachi\"
"
  (let ((url (apply #'format nil (format-string url-rule)
                    (loop for key in (param-keys url-rule)
                          if (eq key :splat)
                            collect (pop (getf params key))
                          else
                            collect (getf params key)
                            and do (remf params key)))))
    (values
     (ppcre:regex-replace-all
      "\\?"
      (ppcre:regex-replace-all "(.\\?)+$" url "") "")
     params)))

@export
(defmethod url-for ((url-rule <regex-url-rule>) params)
  "Return an URL from a rule and parameters.

Example:
    (url-for (make-url-rule \"/hello/:name\")
             '(:name \"fukamachi\"))
    ;=> \"/hello/fukamachi\"
"
  (values (apply #'format nil (format-string url-rule)
                 (getf params :captures))
          (and (remf params :captures) params)))

(doc:start)

@doc:NAME "
Clack.Util.Route - Class for Sinatra-compatible URL rule.
"

@doc:SYNOPSIS "
    (defvar *url-rule* (make-url-rule \"/hello/:name\"))
    
    (match *url-rule* :GET \"/hello/fukamachi\")
    ;=> (NAME \"fukamachi\")
    (match *url-rule* \"/bye/fukamachi\")
    ;=> NIL
    
    (url-for *url-rule* '(:name \"fukamachi\"))
    ;=> \"/hello/fukamachi\"
"

@doc:DESCRIPTION "
Clack.Util.Route provides a Sinatra-compatible routing class.

### Named Parameter

    (match (make-url-rule \"/hello/:name\") :GET \"/hello/fukamachi\")
    ;=> \"/hello/fukamachi\"
        (:NAME \"fukamachi\")

### Wildcard Parameter

    (match (make-url-rule \"/say/*/to/*\") :GET \"/say/hello/to/world\")
    ;=> \"/say/hello/to/world\"
        (:SPLAT (\"hello\" \"world\"))

### Optional Parameter

    (match (make-url-rule \"/?:foo?/?:bar?\") :GET \"/hello/world\")
    ;=> \"/hello/world\"
        (:FOO \"hello\" :BAR \"world\")
    (match (make-url-rule \"/?:foo?/?:bar?\") :GET \"/hello\")
    ;=> \"/hello\"
        (:FOO \"hello\")
    (match (make-url-rule \"/?:foo?/?:bar?\") :GET \"/\")
    ;=> \"/\"
        NIL

### Regular Expression

    (match (make-url-rule \"/hello([\\w]+)\" :regexp t)
           :GET \"/hello/world\")
    ;=> \"/hello/world\"
        (:CAPTURES (\"world\"))
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
* Tomohiro Matsuyama (tomo@cx4a.org)

Note: `compile-rule` was originally written by Tomohiro Matsuyama as `parse-url-rule` in Clack.App.Route.
"
