(in-package :cl-user)
(defpackage t.clack.handler.hunchentoot
  (:use :cl
        :clack.test
        :clack.test.suite))
(in-package :t.clack.handler.hunchentoot)

(clack.test.suite:run-server-tests :hunchentoot)
