(in-package :cl-user)
(defpackage clack-test.handler.wookie
  (:use :cl
        :clack.test.suite
        :rove))
(in-package :clack-test.handler.wookie)

(deftest wookie-tests
  (let ((*error-output* (make-broadcast-stream)))
    (clack.test.suite:run-server-tests :wookie)))
