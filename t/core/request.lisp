(in-package :cl-user)

(defpackage clack-test.request
  (:use :cl
        :cl-test-more
        :flexi-streams
        :clack.request))

(in-package :clack-test.request)

(defvar req
    (make-request `(:content-type "application/x-www-form-urlencoded; charset=utf-8"
                    :query-string "ediweitz=weitzedi"
                    :raw-body
                    ,(flex:make-flexi-stream
                      (flex:make-in-memory-input-stream
                       #(110 97 109 101 61 230 183 177 231 148 186 232 139 177 229 164 170 233 131 142))
                      :external-format :utf-8))))

(plan 3)

(is (content-type req)
    "application/x-www-form-urlencoded; charset=utf-8")

(is (query-parameters req)
    '(:|ediweitz| "weitzedi"))

(is (body-parameters req)
    `(:|name| ,(flex:octets-to-string
                #(230 183 177 231 148 186 232 139 177 229 164 170 233 131 142)
                :external-format :utf-8)))

(is-type (make-request '(:hoge "a")) '<request>)

(finalize)
