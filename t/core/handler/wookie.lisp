(in-package :cl-user)
(defpackage clack-test.handler.wookie
  (:use :cl
        :clack.test.suite))
(in-package :clack-test.handler.wookie)

#+thread-support
(let ((*error-output* (make-broadcast-stream)))
  (clack.test.suite:run-server-tests :wookie))
