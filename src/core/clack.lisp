#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack
  (:use :cl
        :cl-annot.doc)
  (:import-from :clack.component
                :<component>
                :component-designator
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
                :load-handler
                :apply-middleware)
  (:import-from :alexandria
                :delete-from-plist)
  (:import-from :trivial-types
                :pathname-designator)
  (:export :stop
           :<component>
           :<middleware>
           :call
           :call-next
           :make-app
           :wrap))
(in-package :clack)

(cl-syntax:use-syntax :annot)

@export
(defvar *clack-output* *standard-output*
  "Standard output for a Clack running process.")

@export
(defvar *clack-error-output* *error-output*
  "Standard error output for a Clack running process.")

@doc "
Easy way to run Clack Application.
You can specify backend server with passing `:server'. The default is `:hunchentoot'.

Example:
  (clackup (lambda (env)
             (declare (ignore env))
             '(200 nil (\"ok\")))
           :port 5000
           :debug t)

  (clackup #p\"app.lisp\"
           :server :fcgi
           :port 8080
           :debug nil)
"
@export
(defun clackup (app &rest args &key (server :hunchentoot) (port 5000) (debug t) &allow-other-keys)
  (labels ((buildapp (app)
             (reduce #'(lambda (app args)
                         (apply #'apply-middleware app args))
                     `(,app
                       ,@(if debug
                             nil
                             '((:<clack-middleware-backtrace> :clack.middleware.backtrace
                                :result-on-error (500 () ("Internal Server Error")))))
                       (:<clack-middleware-let> :clack.middleware.let
                        :bindings ((*standard-output* *clack-output*)
                                   (*error-output* *clack-error-output*)))
                       (:<clack-middleware-json> :clack.middleware.json)))))
    (etypecase app
      (pathname-designator
       (apply #'clackup
              (buildapp (eval-file app))
              args))
      (component-designator
       (prog1
           (let ((handler-package (find-handler server)))
             (make-instance '<handler>
                            :server-name server
                            :acceptor
                            (apply (intern (string '#:run) handler-package)
                                   (buildapp app)
                                   :port port
                                   :debug debug
                                   (delete-from-plist args :server :port :debug))))
         (format t "~&~:(~A~) server is started.~
             ~%Listening on localhost:~A.~%" server port))))))

(defun eval-file (file)
  "Safer way to read and eval a file content. This function returns the last value."
  (check-type file pathname-designator)
  (with-open-file (in file)
    (let ((*package* *package*)
          (*readtable* *readtable*)
          (*load-pathname* nil)
          (*load-truename* nil))
      (loop with results
            with eof = '#:eof
            for form = (read in nil eof)
            until (eq form eof)
            do (setf results (multiple-value-list (eval form)))
            finally
               (return (apply #'values results))))))

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

    (clackup #p\"app.lisp\"
             :server :fcgi
             :port 8080
             :debug nil)
"

@doc:DESCRIPTION "
Contrary to your expectations, this package is not so significant. Just exporting symbols imported from Clack.Component and Clack.Middleware.

This package is mostly just here to avoid confusion, especially for beginners. Most peaple expect there to be a package that has the same as asdf:system.

Worthy of special mention is `clackup'. It provides an easy way to run Clack Applications.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Component
* Clack.Middleware
"
