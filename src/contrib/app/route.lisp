#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>
  Copyright (c) 2011 Tomohiro Matsuyama <tomo@cx4a.org>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.app.route
  (:use :cl
        :clack)
  (:import-from :cl-annot.eval-when
                :eval-always)
  (:import-from :cl-annot.doc
                :doc)
  (:import-from :cl-ppcre
                :scan-to-strings
                :split
                :quote-meta-chars)
  (:import-from :alexandria
                :with-gensyms)
  (:import-from :clack.util.route
                :make-url-rule
                :match))

(cl-syntax:use-syntax :annot)

@eval-always
@doc "
[DEPRECATED]
Parse an URL rule and return a list of (regex control-string
variables).

Example:
  (parse-url-rule \"/login\")
  ;;=> (\"^\\/login$\" \"/login\" NIL)
  (parse-url-rule \"/member/:id\")
  ;;=> (\"^\\/member\\/(.+?)$\" \"/member/~A\" (ID))
"
@export
(defun parse-url-rule (url)
  (loop with list = (split ":([\\w-]+)" url :with-registers-p t)
        while list
        for prefix = (pop list)
        for name = (pop list)
        collect prefix into re
        collect prefix into cs
        if name
          collect (intern (string-upcase name)) into names
          and collect "(.+?)" into re
          and collect "~A" into cs
        finally
     (return (list (format nil "^~{~A~}$" re)
                   (format nil "~{~A~}" cs)
                   names))))

@export
(defmacro defroutes (name &body routes &aux (otherwise (last routes)))
  (if (member (car otherwise) '(t otherwise))
      (setf routes (butlast routes))
      (setf otherwise nil))
  (with-gensyms (env request-method request-path matched params)
    `(defun ,name (,env)
       (let ((,request-method (getf ,env :request-method))
             (,request-path (getf ,env :path-info)))
         (declare (ignorable ,request-method ,request-path))
         (or ,@(loop for (method path form) in routes
                     collect `(when (string= ,request-method ',method)
                                (multiple-value-bind (,matched ,params)
                                    (match (make-url-rule ,path) ,request-path)
                                  (declare (ignorable ,params))
                                  (when ,matched
                                    ,(if params
                                         `(let (,@(loop for (k v) on params by #'cddr
                                                        collect (list (intern (string k)) v)))
                                            (declare (ignorable ,@(loop for k in params by #'cddr collect k)))
                                            (call ,form ,env))
                                         `(call ,form ,env))))))
             ,(if otherwise
                  `(call ,(cadr otherwise) ,env)
                  '(list 404 nil nil)))))))

(doc:start)

@doc:NAME "
Clack.App.Route - URL dispatcher.
"

@doc:SYNOPSIS "
    (defpackage clack-sample
      (:use :cl
            :clack
            :clack.app.route))
    (in-package :clack-sample)
    
    (defroutes app (env)
      (GET \"/\" #'index)
      (GET \"/login\" #'login)
      (POST \"/login\" #'authorize)
      (GET \"/member/:id\" #'member))
    
    (clackup #'app)
"

@doc:DESCRIPTION "
Clack.App.Route provides an URL based dispacher, inspired by Ruby's Sinatra.

This package is using Clack.Util.Route to parse rule strings.

Note: Though `parse-url-rule` is provided from this package now, it is now deprecated. Use Clack.Util.Route for instead.
"

@doc:AUTHOR "
* Tomohiro Matsuyama (tomo@cx4a.org)
"

@doc:CONTRIBUTORS "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Util.Route
"
