(in-package :cl-user)

(defpackage clack-test.handler.hunchentoot
  (:use :cl
        :clack.test.suite))

(in-package :clack-test.handler.hunchentoot)

(clack.test.suite:run-server-tests :hunchentoot)
