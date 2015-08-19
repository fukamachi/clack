(in-package :cl-user)
(defpackage clack-test.handler.fcgi
  (:use :cl
        :clack.test
        :clack.test.suite))
(in-package :clack-test.handler.fcgi)

(let ((clack.test:*clack-test-port* 14949)
      (clack.test:*clack-test-access-port* 4949))
  (clack.test.suite:run-server-tests :fcgi))
