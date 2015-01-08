#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.test
  (:use :cl)
  (:import-from :cl-test-more
                :diag
                :deftest)
  (:import-from :clack.util
                :find-handler))
(in-package :clack.test)

(cl-syntax:use-syntax :annot)

@export
(defvar *clack-test-handler* :hunchentoot
  "Backend Handler to run tests on. String or Symbol are allowed.")

@export
(defvar *clack-test-port* 4242
  "HTTP port number of Handler.")

@export
(defvar *enable-debug-p* t)

@export
(defun test-app (app client &optional desc)
  "Test Clack Application."
  (let* ((handler (find-handler *clack-test-handler*))
         (debug *enable-debug-p*)
         (acceptor (bt:make-thread
                    (lambda ()
                      (funcall (intern (string '#:run) handler)
                               app
                               :port *clack-test-port*
                               :debug debug)))))
    (when desc (diag desc))
    (sleep 0.5)
    (unwind-protect
        (funcall client)
      (when (bt:thread-alive-p acceptor)
        (bt:destroy-thread acceptor))
      (sleep 0.5))))

@export
(defmacro define-app-test (desc app client &optional (enable-debug-p *enable-debug-p*))
  "Define tests for Clack Application. This just wrap `cl-test-more:deftest', so you can run this test by calling `(cl-test-more:run-test :foo)'."
  `(deftest ,desc
       (let ((*enable-debug-p* ,enable-debug-p))
         (test-app ,app ,client))))

(doc:start)

@doc:NAME "
Clack.Test - Testing Clack Applications.
"

@doc:SYNOPSIS "
    (defpackage clack-test.sample
      (:use :cl
            :clack.test
            :cl-test-more
            :drakma))
    (in-package :clack-test.sample)
    
    (test-app
     (lambda (env)
       (declare (ignore env))
       `(200 (:content-type \"text/plain\") (\"Hello, Clack!\")))
     (lambda ()
       (multiple-value-bind (body status headers)
           (http-request \"http://localhost:4242\")
         (is status 200)
         (is body \"Hello, Clack!\")
         (is (assoc :content-type headers) \"text/plain\")))
     \"Testing simple application\")
"

@doc:DESCRIPTION "
Clack.Test provides simple interface to test Clack Application or Middleware.

Your Lisp have to support multi-thread to run this.
"

@doc:AUTHOR "
Eitaro Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* [CL-TEST-MORE](https://github.com/fukamachi/cl-test-more)
* Drakma
"
