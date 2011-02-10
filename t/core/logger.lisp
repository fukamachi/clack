(clack.util:namespace clack-test.logger
  (:use :cl
        :cl-test-more
        :clack.logger))

(plan nil)
(setf *logger-min-level* clack.logger:+warning+)
(setf *logger-output* nil)
(diag "Log min level: [warning]")
(is (log-message :notice "hoge") nil "notice")
(like (log-message :warning "fuga") "\\[WARNING\\] fuga$" "warning")
(like (log-message :emergency "piyo") "\\[EMERGENCY\\] piyo$" "emergency")

(finalize)
