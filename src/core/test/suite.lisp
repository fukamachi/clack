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

(defun test (fn app &optional desc)
  "Test each registed tests."
  (let ((acceptor (funcall (intern "RUN" *handler-package*)
                           app :port 4242 :debug t)))
    (when desc (diag desc))
    (funcall fn)
    (funcall (intern "STOP" *handler-package*) acceptor)))

(defmacro deftest (desc fn app)
  "Regist a test. Note that `desc' should be a string."
  `(push (list ,fn ,app ,desc) *tests*))

;; Tests

(deftest "normal"
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/")
      (is body (format nil "ok~%"))))
  (lambda (req)
    (declare (ignore req))
    '(200
      (:content-type "text/plain")
      ("ok"))))
