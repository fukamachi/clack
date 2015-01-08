(in-package :cl-user)
(defpackage clack-test.handler.fcgi
  (:use :cl
        :clack.test.suite))
(in-package :clack-test.handler.fcgi)

(let ((clack.test:*clack-test-port* 14242)
      (clack.test.suite:*clack-test-access-port* 4242))
  (clack.test.suite:run-server-tests :fcgi))
