(in-package :cl-user)

(defpackage clack-test.handler.toot
  (:use :cl
        :prove
        :clack.test.suite))

(in-package :clack-test.handler.toot)

#+thread-support
(let ((*error-output* (make-broadcast-stream)))
  (clack.test.suite:run-server-tests :toot))
#-thread-support
(progn
  (plan 1)
  (skip 1 "because your Lisp doesn't support threads")
  (finalize))
