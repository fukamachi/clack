(in-package :cl-user)
(defpackage t.clack.request
  (:use :cl
        :asdf
        :prove
        :flexi-streams
        :clack.request
        :clack.test
        :drakma))
(in-package :t.clack.request)

(defvar req nil)

(setq req
      (make-request `(:content-type "application/x-www-form-urlencoded; charset=utf-8"
                      :uri-scheme :http
                      :query-string "ediweitz=weitzedi&name=eitaro&q=C%2B%2B"
                      :raw-body
                      ,(flex:make-flexi-stream
                        (flex:make-in-memory-input-stream
                         #(110 97 109 101 61 230 183 177 231 148 186 232 139 177 229 164 170 233 131 142))
                        :external-format :utf-8)
                      :headers ,(alexandria:plist-hash-table
                                 (list "referer" "http://github.com/fukamachi/clack"
                                       "user-agent" "Mozilla/5.0 (Macintosh; U; Intel Mac OS X 10_6_6; en-US)"
                                       "cookie" "hoge=1;fuga=semi;colon")
                                 :test 'equal))))

(plan 16)

(ok (env req) "env")

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
    '(("ediweitz" . "weitzedi") ("name" . "eitaro") ("q" . "C++"))
    "query-parameter")

(is (query-parameter req "q")
    "C++"
    "query-parameter (accessing each field)")

(is (body-parameter req)
    `(("name" . ,(flex:octets-to-string
                  #(230 183 177 231 148 186 232 139 177 229 164 170 233 131 142)
                  :external-format :utf-8)))
    "body-parameter")

(is (body-parameter req "name")
    (flex:octets-to-string
     #(230 183 177 231 148 186 232 139 177 229 164 170 233 131 142)
     :external-format :utf-8)
    "body-parameter (accessing each field)")

(let* ((body (flex:make-in-memory-input-stream (flex:string-to-octets "foo=bar")))
       (env `(:raw-body ,body :content-type "application/x-www-form-urlencoded"))
       (req1 (make-request env))
       (req2 (make-request env)))
  (is (body-parameter req1 "foo")
      "bar"
      "body-parameter (sharing)")
  (is (body-parameter req2 "foo")
      "bar"
      "body-parameter (confirm sharing)"))

(is (parameter req)
    `(("ediweitz" . "weitzedi")
      ("name" . "eitaro")
      ("q" . "C++")
      ("name" . ,(flex:octets-to-string
                  #(230 183 177 231 148 186 232 139 177 229 164 170 233 131 142)
                  :external-format :utf-8)))
    "parameter")

(is-type (make-request '(:hoge "a")) '<request> "<request> allow other keys")

(is (cookies req) '(("hoge" . "1") ("fuga" . "semi") ("colon")) "cookies")
(is (cookies req "hoge") "1" "cookie value")

(diag "file upload")

(defvar *clack-pathname*
    (asdf:component-pathname (asdf:find-system :clack)))

#+thread-support
(subtest-app "make-request"
    (lambda (env)
      (make-request env)
      `(200 nil (,(gethash "filename" (cadr (body-parameter (make-request env) :|file|))))))
  (multiple-value-bind (body status)
      (http-request (localhost)
                    :method :post
                    :parameters
                    `(("file" ,(merge-pathnames #p"tmp/jellyfish.jpg" *clack-pathname*)
                              :content-type "image/jpeg"
                              :filename "jellyfish.jpg")))
    (is status 200)
    (is body "jellyfish.jpg"))

  (multiple-value-bind (body status)
      (http-request (localhost)
                    :method :post
                    :parameters
                    `(("file" ,(merge-pathnames #p"tmp/jellyfish.jpg" *clack-pathname*)
                              :content-type "image/jpeg"
                              :filename "jellyfish.jpg"))
                    :content-length t)
    (is status 200)
    (is body "jellyfish.jpg")))
#-thread-support
(skip 4 "because your lisp doesn't support threads")

(finalize)
