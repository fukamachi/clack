(in-package :cl-user)
(defpackage t.clack.logger
  (:use :cl
        :prove
        :clack.logger))
(in-package :t.clack.logger)

(plan nil)
(setf *logger-min-level* clack.logger:+warning+)
(setf *logger-output* nil)
(diag "Log min level: [warning]")
(is (log-message :notice "hoge") nil "notice")
(like (log-message :warning "fuga") "\\[WARNING\\] fuga$" "warning")
(like (log-message :emergency "piyo") "\\[EMERGENCY\\] piyo$" "emergency")

(finalize)
