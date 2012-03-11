(in-package :cl-user)
(defpackage t.clack.handler.apache
  (:use :cl
        :clack.test.suite))
(in-package :t.clack.handler.apache)

#+thread-support
(clack.test.suite:run-server-tests :apache)
