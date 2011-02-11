(in-package :cl-user)

(defpackage clack-test.request
  (:use :cl
        :asdf
        :cl-test-more
        :flexi-streams
        :clack.request
        :clack.test
        :drakma))

(in-package :clack-test.request)

(defvar req nil)

(setq req
      (make-request `(:content-type "application/x-www-form-urlencoded; charset=utf-8"
                      :uri-scheme :http
                      :http-referer "http://github.com/fukamachi/clack"
                      :http-user-agent "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_6; en-US)"
                      :http-cookie "hoge=1&fuga=2"
                      :query-string "ediweitz=weitzedi&q=C%2B%2B"
                      :raw-body
                      ,(flex:make-flexi-stream
                        (flex:make-in-memory-input-stream
                         #(110 97 109 101 61 230 183 177 231 148 186 232 139 177 229 164 170 233 131 142))
                        :external-format :utf-8))))

(plan 13)

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

(is (query-parameters req "q")
    "C++"
    "query-parameters (accessing each field)")

(is (body-parameters req)
    `(:|name| ,(flex:octets-to-string
                #(230 183 177 231 148 186 232 139 177 229 164 170 233 131 142)
                :external-format :utf-8))
    "body-parameters")

(is (body-parameters req "name")
    (flex:octets-to-string
     #(230 183 177 231 148 186 232 139 177 229 164 170 233 131 142)
     :external-format :utf-8)
    "body-parameters (accessing each field)")

(is-type (make-request '(:hoge "a")) '<request> "<request> allow other keys")

(is (cookies req) '(:|hoge| "1" :|fuga| "2") "cookies")
(is (cookies req "hoge") "1" "cookie value")

(diag "file upload")

(defvar *clack-pathname*
    (asdf:component-pathname (asdf:find-system :clack)))

(setf clack.test:*clack-test-port* 4242)

(test-app
 (lambda (req)
   `(200 nil (,(caddar (uploads (make-request req))))))
 (lambda ()
   (multiple-value-bind (body status)
       (http-request "http://localhost:4242/"
                     :method :post
                     :parameters `(("file" ,(merge-pathnames #p"tmp/jellyfish.jpg" *clack-pathname*) :content-type "image/jpeg" :filename "jellyfish.jpg")))
     (is status 200)
     (is body "jellyfish.jpg"))))

(finalize)
