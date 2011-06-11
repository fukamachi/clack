#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack
  (:use :cl
        :cl-annot.doc)
  (:import-from :clack.component
                :<component>
                :call
                :make-app)
  (:import-from :clack.middleware
                :<middleware>
                :call-next
                :wrap)
  (:import-from :clack.util
                :find-handler)
  (:export :<component>
           :<middleware>
           :call
           :call-next
           :make-app
           :wrap))

(cl-annot:enable-annot-syntax)

@doc "
Easy way to run Clack Application.
You can specify backend server with passing `:server'. The default is `:hunchentoot'.

Example:
  (clackup (lambda (env)
             (declare (ignore env))
             '(200 nil (\"ok\")))
           :port 5000
           :debug t)
"
@export
(defun clackup (app &key (server :hunchentoot) (port 5000) (debug t))
  (prog1
    (let ((handler (find-handler server)))
      (funcall (intern "RUN" handler)
               app
               :port port
               :debug debug))
    (format t "~&~:(~A~) server is started.~
             ~%Listening on localhost:~A.~%" server port)))

@export
(defun stop (handler &key (server :hunchentoot))
  "Stop Clack server. Currently works only Hunchentoot."
  (let ((handler-package (find-handler server)))
    (funcall (intern "STOP" handler-package) handler)))

(doc:start)

@doc:NAME "
Clack main package just for convenience.
"

@doc:SYNOPSIS "
    (clackup (lambda (env)
               (declare (ignore env))
               '(200 nil (\"Hello, Clack!\")))
             :port 5000
             :debug t)
"

@doc:DESCRIPTION "
Contrary to your expectations, this package is not so significant. Just exporting symbols imported from Clack.Component and Clack.Middleware.

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
