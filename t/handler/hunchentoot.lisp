(in-package :cl-user)
(defpackage t.clack.handler.hunchentoot
  (:use :cl
        :clack.test
        :clack.test.suite
        :rove))
(in-package :t.clack.handler.hunchentoot)

(deftest hunchentoot-tests
  (clack.test.suite:run-server-tests :hunchentoot))
