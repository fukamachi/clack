#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack
  (:use :cl
        :cl-annot.doc
        :split-sequence)
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
  (:import-from :clack.file-watcher
                :watch-systems)
  (:import-from :clack.util
                :find-handler
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
(defvar *clack-output* '*standard-output*
  "Standard output for a Clack running process.")

@export
(defvar *clack-error-output* '*error-output*
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
(defun clackup (app &rest args
                &key
                  (server :hunchentoot)
                  (port 5000)
                  (debug t)
                  watch
                  (use-thread t)
                  (use-cl-debugger t)
                  (use-default-middlewares t)
                &allow-other-keys)
  (when (find :shelly *features* :test #'eq)
    (setf use-cl-debugger nil))
  (unless use-cl-debugger
    #+quicklisp (ql:quickload :clack-errors)
    #-quicklisp (asdf:load-system :clack-errors))
  (labels ((buildapp (app)
             (reduce #'(lambda (app args)
                         (apply #'apply-middleware app args))
                     `(,app
                       ,@(cond
                           ((and debug use-cl-debugger) nil)
                           (debug '((:<clack-error-middleware> :clack-errors)))
                           (T '((:<clack-middleware-backtrace> :clack.middleware.backtrace
                                 :result-on-error (500 () ("Internal Server Error")))))))))
           (run-server (handler-package app)
             (apply (intern (string '#:run) handler-package)
                    (if use-default-middlewares
                        (buildapp app)
                        app)
                    :port port
                    :debug debug
                    :allow-other-keys t
                    (delete-from-plist args :server :port :debug :watch
                                            :use-thread
                                            :use-cl-debugger
                                            :use-default-middlewares))))
    (etypecase app
      (pathname-designator
       (apply #'clackup (eval-file app) args))
      (component-designator
       (let ((handler-package (find-handler server)))
         (unless use-thread
           (format t "~&~:(~A~) server is starting.~
             ~%Listening on localhost:~A.~%" server port)
           (run-server handler-package app))

         (let ((handler (make-instance '<handler>
                                       :server-name server
                                       :acceptor
                                       (bt:make-thread
                                        (lambda ()
                                          (run-server handler-package app))
                                        :name (format nil "clack-handler-~(~A~)" server)
                                        :initial-bindings `((*standard-output* . ,(if (symbolp *clack-output*)
                                                                                      (symbol-value *clack-output*)
                                                                                      *clack-output*))
                                                            (*error-output* . ,(if (symbolp *clack-error-output*)
                                                                                   (symbol-value *clack-error-output*)
                                                                                   *clack-error-output*)))))))
           (sleep 1)
           (format t "~&~:(~A~) server is started.~
             ~%Listening on localhost:~A.~%" server port)
           (when watch
             (when (stringp watch)
               (setf watch (split-sequence #\, watch)))
             (watch-systems handler watch))
           handler))))))

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
* Eitaro Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Component
* Clack.Middleware
"
