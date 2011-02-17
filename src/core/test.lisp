#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.test
  (:use :cl)
  (:import-from :cl-test-more
                :diag
                :deftest))

(cl-annot:enable-annot-syntax)

@export
(defvar *clack-test-handler* 'clack.handler.hunchentoot
  "Backend Handler to run tests on. String or Symbol are allowed.")

@export
(defvar *clack-test-port* 4242
  "HTTP port number of Handler.")

@export
(defvar *enable-debug-p* t)

@export
(defun test-app (app client &optional desc)
  "Test Clack Application."
  (let* ((handler
          (or (find-package *clack-test-handler*)
              (error "Handler package is not found. Forgot to load it?: ~A"
                     *clack-test-handler*)))
         (acceptor (funcall (intern "RUN" handler)
                            app
                            :port *clack-test-port*
                            :debug *enable-debug-p*)))
    (when desc (diag desc))
    (unwind-protect
        (funcall client)
      (funcall (intern "STOP" handler) acceptor))))

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
     (lambda (req)
       (declare (ignore req))
       `(200 (:content-type \"text/plain\") (\"Hello, Clack!\")))
     (lambda ()
       (multiple-value-bind (body status headers)
           (http-request \"http://localhost:4242\")
         (is status 200)
         (is body \"Hello, Clack!\")
         (is (getf headers :content-type) \"text/plain\")))
     \"Testing simple application\")
"

@doc:DESCRIPTION "
Clack.Test provides simple interface to test Clack Application or Middleware.

Your Lisp have to support multi-thread to run this.
"

@doc:AUTHOR "
Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* [CL-TEST-MORE](https://github.com/fukamachi/cl-test-more)
* Drakma
"
