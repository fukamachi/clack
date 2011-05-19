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

(plan 15)

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

(is (query-parameter req)
    '(:|ediweitz| "weitzedi" :|q| "C++")
    "query-parameter")

(is (query-parameter req "q")
    "C++"
    "query-parameter (accessing each field)")

(is (body-parameter req)
    `(:|name| ,(flex:octets-to-string
                #(230 183 177 231 148 186 232 139 177 229 164 170 233 131 142)
                :external-format :utf-8))
    "body-parameter")

(is (body-parameter req "name")
    (flex:octets-to-string
     #(230 183 177 231 148 186 232 139 177 229 164 170 233 131 142)
     :external-format :utf-8)
    "body-parameter (accessing each field)")

(let* ((body (flex:make-in-memory-input-stream (flex:string-to-octets "foo=bar")))
       (req `(:raw-body ,body :content-type "application/x-www-form-urlencoded"))
       (req1 (make-request req))
       (req2 (make-request req)))
  (is (body-parameter req1 "foo")
      "bar"
      "body-parameter (sharing)")
  (is (body-parameter req2 "foo")
      "bar"
      "body-parameter (confirm sharing)"))

(is-type (make-request '(:hoge "a")) '<request> "<request> allow other keys")

(is (cookies req) '(:|hoge| "1" :|fuga| "2") "cookies")
(is (cookies req "hoge") "1" "cookie value")

(diag "file upload")

(defvar *clack-pathname*
    (asdf:component-pathname (asdf:find-system :clack)))

#+thread-support
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
#-thread-support
(skip 2 "because your lisp doesn't support threads")

(finalize)
