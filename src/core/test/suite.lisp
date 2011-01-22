#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Test suite for Clack handlers.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)

(defpackage clack.test.suite
  (:use :cl
        :anaphora
        :drakma
        :cl-test-more)
  (:export :run-server-tests))

(in-package :clack.test.suite)

(defvar *handler-package* nil
  "Handler package to test.")
(defvar *tests* '()
  "Collection of tests for Clack Handler.")

(defun run-server-tests (handler-name)
  "Run tests for clack.handler.
Handler name is a keyword and doesn't include the clack.handler prefix.
For example, if you have a handler `clack.handler.foo',
you would call like this: `(run-server-tests :foo)'."
  (setf *handler-package*
        (find-package
         (concatenate 'string "CLACK.HANDLER."
                      (symbol-name handler-name))))
  (plan (length *tests*))
  (dolist (test *tests*)
    (apply #'test test))
  (finalize))

(defun test (desc fn app)
  "Test each registed tests."
  (let ((acceptor (funcall (intern "RUN" *handler-package*)
                           app :port 4242 :debug t)))
    (when desc (diag desc))
    (funcall fn)
    (funcall (intern "STOP" *handler-package*) acceptor)))

(defmacro deftest (desc fn app)
  "Regist a test. Note that `desc' should be a string."
  (let ((test (gensym "TEST")))
    `(let ((,test (list ,desc ,fn ,app)))
       (sif (member ,desc *tests*
                    :key #'car
                    :test #'string=)
            (rplaca it ,test)
            (push ,test *tests*)))))

;; Tests

(deftest "SCRIPT-NAME"
  (lambda ()
    (is (http-request "http://localhost:4242/") nil))
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :script-name)))))

(deftest "GET"
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/?name=fukamachi")
      (is status 200)
      (is (cdr (assoc :content-type headers))
          "text/plain")
      (is body "Hello, name=fukamachi")))
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(format nil "Hello, ~A" (getf req :query-string))))))
