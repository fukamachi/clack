(clack.util:namespace clack-test.util.route
  (:use :cl
        :clack.util.route
        :cl-test-more))

(plan 5)

(defvar *url-rule*)

(diag "normal case")
(setf *url-rule* (make-url-rule "/hello"))

(is (match *url-rule* "/hello") "/hello" "'/hello' matches '/hello'")
(is (match *url-rule* "/bye") nil "'/hello' doesn't match '/bye'")

(diag "with named parameter")
(setf *url-rule* (make-url-rule "/hello/:name"))

(is (multiple-value-list
     (match *url-rule* "/hello/fukamachi"))
    '("/hello/fukamachi" (:name "fukamachi"))
    "match")

(is (multiple-value-list
     (match *url-rule* "/hello/fukamachi/eitarow"))
    '("/hello/fukamachi/eitarow" (:name "fukamachi/eitarow"))
    "containing a slash")

(is (multiple-value-list
     (match *url-rule* "/bye/fukamachi"))
    '(nil nil)
    "not match")

(finalize)
