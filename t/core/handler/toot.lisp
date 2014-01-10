(in-package :cl-user)

(defpackage clack-test.handler.toot
  (:use :cl
        :clack.test.suite))

(in-package :clack-test.handler.toot)

#+thread-support
(let ((*error-output* (make-broadcast-stream)))
  (clack.test.suite:run-server-tests :toot))
