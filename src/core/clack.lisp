#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack
  (:use :cl)
  (:import-from :clack.component
                :<component>
                :call
                :make-app)
  (:import-from :clack.middleware
                :<middleware>
                :call-next
                :wrap)
  (:export :<component>
           :<middleware>
           :call
           :call-next
           :make-app
           :wrap))

(cl-annot:enable-annot-syntax)

@export
(defun clackup (app &key (server :hunchentoot) (port 8080) debug)
  "Easy way to run Clack Application.
You can specify backend server with passing `:server'. The default is `:hunchentoot'.

Example:
  (clackup (lambda (req)
             (declare (ignore req))
             '(200 nil (\"ok\")))
           :port 8080
           :debug t)
"
  (let* ((handler-name (concatenate 'string
                                    "CLACK.HANDLER."
                                    (symbol-name server)))
         (handler (or (find-package handler-name)
                      (error "Handler package is not found. Forgot to load it?: ~A"
                             handler-name))))
    (funcall (intern "RUN" handler) app
             :port port
             :debug debug)))

(doc:start)

@doc:NAME "
Clack main package just for convenience.
"

@doc:SYNOPSIS "
    (clackup (lambda (req)
               (declare (ignore req))
               '(200 nil (\"Hello, Clack!\")))
             :port 8080
             :debug t)
"

@doc:DESCRIPTION "
Contrary to your expextations, this package is not so significant. Just exporting symbols imported from Clack.Component and Clack.Middleware.

To avoid confusion, especially for beginner, why this package is. Most of peaple expect that there is a package that has the same as asdf:system.

Worthy of special mention is `clackup'. It provides easy way to run Clack Applications.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Component
* Clack.Middleware
"
