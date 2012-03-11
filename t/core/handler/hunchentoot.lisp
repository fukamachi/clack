(in-package :cl-user)
(defpackage t.clack.handler.hunchentoot
  (:use :cl
        :clack.test.suite))
(in-package :t.clack.handler.hunchentoot)

#+thread-support
(clack.test.suite:run-server-tests :hunchentoot)
