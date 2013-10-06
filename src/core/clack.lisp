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
  (:import-from :clack.util.stream
                :slurp-stream-to-string)
  (:import-from :alexandria
                :delete-from-plist)
  (:import-from :trivial-types
                :pathname-designator)
  (:import-from :bordeaux-threads
                :make-thread
                :thread-alive-p)
  (:export :stop
           :<component>
           :<middleware>
           :call
           :call-next
           :make-app
           :wrap))

(cl-syntax:use-syntax :annot)

@export
(defvar *clack-output* *standard-output*
  "Standard output for a Clack running process. This variable will be used in `<clack-middleware-stdout>'.")

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
  (etypecase app
    (pathname-designator
     (apply #'clackup
            (eval-file app)
            args))
    (component-designator
     (prog1
         (let ((handler-package (find-handler server)))
           (make-instance '<handler>
                          :server-name server
                          :acceptor
                          (apply (intern (string '#:run) handler-package)
                                 (apply-middleware (apply-middleware app
                                                                     :<clack-middleware-stdout>
                                                                     :clack.middleware.stdout
                                                                     :standard-output '*clack-output*)
                                                   :<clack-middleware-json>
                                                   :clack.middleware.json)
                                 :port port
                                 :debug debug
                                 (delete-from-plist args :server :port :debug))))
       (format t "~&~:(~A~) server is started.~
             ~%Listening on localhost:~A.~%" server port)))))

(defun eval-file (file)
  "Safer way to read and eval a file content. This function returns the last value."
  (check-type file pathname-designator)
  (loop with retval = nil
        with content = (with-open-file (in file :element-type '(unsigned-byte 8))
                         (clack.util.stream:slurp-stream-to-string in))
        with thread = (bt:make-thread
                       (lambda ()
                         (setf retval (eval (with-standard-io-syntax
                                              (read-from-string
                                               (format nil "(progn ~A)" content)))))))
        while (bt:thread-alive-p thread)
        finally (return retval)))

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
