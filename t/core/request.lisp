(in-package :cl-user)

(defpackage clack-test.request
  (:use :cl
        :cl-test-more
        :flexi-streams
        :clack.request))

(in-package :clack-test.request)

(defvar req nil)

(setq req
      (make-request `(:content-type "application/x-www-form-urlencoded; charset=utf-8"
                      :uri-scheme :http
                      :http-referer "http://github.com/fukamachi/clack"
                      :http-user-agent "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_6; en-US)"
                      :query-string "ediweitz=weitzedi&q=C%2B%2B"
                      :raw-body
                      ,(flex:make-flexi-stream
                        (flex:make-in-memory-input-stream
                         #(110 97 109 101 61 230 183 177 231 148 186 232 139 177 229 164 170 233 131 142))
                        :external-format :utf-8))))

(plan 9)

(is (content-type req)
    "application/x-www-form-urlencoded; charset=utf-8"
    "content-type")

(is (securep req) nil "securep")

(is (referer req)
    "http://github.com/fukamachi/clack"
    "referer")

(is (user-agent req)
    "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_6; en-US)"
    "user-agent")

(is (query-parameters req)
    '(:|ediweitz| "weitzedi" :|q| "C++")
    "query-parameters")

(is (query-parameter req "q")
    "C++"
    "query-parameter (accessing each field)")

(is (body-parameters req)
    `(:|name| ,(flex:octets-to-string
                #(230 183 177 231 148 186 232 139 177 229 164 170 233 131 142)
                :external-format :utf-8))
    "body-parameters")

(is (body-parameter req "name")
    (flex:octets-to-string
     #(230 183 177 231 148 186 232 139 177 229 164 170 233 131 142)
     :external-format :utf-8)
    "body-parameter (accessing each field)")

(is-type (make-request '(:hoge "a")) '<request> "<request> allow other keys")

(finalize)
