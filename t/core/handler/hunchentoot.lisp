(in-package :cl-user)
(defpackage t.clack.handler.hunchentoot
  (:use :cl
        :prove
        :clack.test.suite))
(in-package :t.clack.handler.hunchentoot)

#+thread-support
(clack.test.suite:run-server-tests :hunchentoot)
#-thread-support
(progn
  (plan 1)
  (skip 1 "because your Lisp doesn't support threads")
  (finalize))
