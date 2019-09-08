(in-package :cl-user)

(defpackage clack-test.handler.toot
  (:use :cl
        :clack.test.suite
        :rove))

(in-package :clack-test.handler.toot)

(deftest toot-tests
  (let ((*error-output* (make-broadcast-stream)))
    (clack.test.suite:run-server-tests :toot)))
