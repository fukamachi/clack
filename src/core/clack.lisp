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
  (:import-from :clack.handler
                :<handler>
                :stop)
  (:import-from :clack.util
                :find-handler
                :load-handler)
  (:export :stop
           :<component>
           :<middleware>
           :call
           :call-next
           :make-app
           :wrap))

(cl-syntax:use-syntax :annot)

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
    (let ((handler-package (find-handler server)))
      (make-instance '<handler>
         :server-name server
         :acceptor
         (funcall (intern "RUN" handler-package)
                  app
                  :port port
                  :debug debug)))
    (format t "~&~:(~A~) server is started.~
             ~%Listening on localhost:~A.~%" server port)))

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
