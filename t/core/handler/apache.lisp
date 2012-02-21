(in-package :cl-user)

(defpackage clack-test.handler.apache
  (:use :cl
        :clack.test.suite))

(in-package :clack-test.handler.apache)

#+thread-support
(clack.test.suite:run-server-tests :apache)
