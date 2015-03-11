#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.test.suite
  (:use :cl
        :prove)
  (:import-from :flexi-streams
                :octet
                :octets-to-string)
  (:import-from :clack.test
                :*clack-test-handler*
                :*clack-test-port*
                :define-app-test)
  (:import-from :asdf
                :find-system
                :component-pathname))
(in-package :clack.test.suite)

(cl-syntax:use-syntax :annot)

@export
(defvar *clack-test-access-port* *clack-test-port*
  "Port of localhost to request.
Use if you want to set another port. The default is `*clack-test-port*`.")

(defvar *clack-pathname*
    (asdf:component-pathname (asdf:find-system :clack)))

@export
(defun run-server-tests (handler-name &optional name)
  "Run tests for clack.handler.
Handler name is a keyword and doesn't include the clack.handler prefix.
For example, if you have a handler `clack.handler.foo',
you would call like this: `(run-server-tests :foo)'."
  (let ((*clack-test-handler* handler-name)
        (dex:*use-connection-pool* nil)
        (*package* (find-package :clack.test.suite)))
    #+thread-support
    (if name
        (run-test name)
        (progn
          (plan 34)
          (run-test-package :clack.test.suite)))
    #-thread-support
    (progn
      (plan (if name 1 32))
      (skip (if name 1 32) "because your Lisp doesn't support threads")
      (finalize))))

(defun get-header (headers key)
  (let ((val (assoc key headers)))
    (values (cdr val) (not (null val)))))

(defun file-size (file)
  (with-open-file (in file :direction :input)
    (file-length in)))

(defun localhost (&optional path)
  (format nil
          "http://localhost:~D/~:[~;~:*~A~]"
          *clack-test-access-port*
          path))

;; Tests

(define-app-test |list body|
  (lambda (env)
    (declare (ignore env))
    '(200 (:content-type "text/plain") ("Hello" "World")))
  (lambda ()
    (multiple-value-bind (body status)
        (dex:get (localhost))
      (is status 200)
      (is body "HelloWorld"))))

(define-app-test |SCRIPT-NAME|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(getf env :script-name))))
  (lambda ()
    (ok (member (dex:get (localhost)) '(nil "") :test #'equal))))

(define-app-test |GET|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(format nil "Hello, ~A" (getf env :query-string)))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (dex:get (localhost "?name=fukamachi"))
      (is status 200)
      (is (gethash "content-type" headers)
          "text/plain; charset=utf-8")
      (is body "Hello, name=fukamachi"))))

(define-app-test |POST|
  (lambda (env)
    (let ((body (make-array 11 :element-type '(unsigned-byte 8))))
      (read-sequence body (getf env :raw-body))
      `(200
        (:content-type "text/plain; charset=utf-8"
         :client-content-length ,(getf env :content-length)
         :client-content-type ,(getf env :content-type))
        (,(format nil "Hello, ~A" (babel:octets-to-string body))))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (dex:post (localhost)
                  :content '(("name" . "eitaro")))
      (is status 200)
      (is (gethash "client-content-length" headers) 11)
      (is (gethash "client-content-type" headers) "application/x-www-form-urlencoded")
      (is body "Hello, name=eitaro"))))

(define-app-test |big POST|
  (lambda (env)
    (let ((body
           (make-array (getf env :content-length)
                       :element-type 'octet)))
      (read-sequence body (getf env :raw-body))
      `(200
        (:content-type "text/plain; charset=utf-8"
         :client-content-length ,(getf env :content-length)
         :client-content-type ,(getf env :content-type))
        (,(flex:octets-to-string body)))))
  (lambda ()
    (let* ((chunk
            (with-output-to-string (chunk)
              (dotimes (i 12000) (write-string "abcdefgh" chunk))
              chunk))
           (len (length chunk)))
      (multiple-value-bind (body status headers)
          (dex:post (localhost)
                    :headers `((:content-type . "application/octet-stream")
                               (:content-length . ,len))
                    :content `((,chunk)))
        (is status 200)
        (is (gethash "client-content-length" headers)
            len)
        (is (length body) len)))))

(define-app-test |url-scheme|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(string (getf env :url-scheme)))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (dex:post (localhost))
      (is status 200)
      (is (gethash "content-type" headers) "text/plain; charset=utf-8")
      (is body "HTTP"))))

(define-app-test |return pathname|
  (lambda (env)
    @ignore env
    `(200
      (:content-type "text/plain; charset=utf-8")
      ,(merge-pathnames #p"tmp/file.txt" *clack-pathname*)))
  (lambda ()
      (multiple-value-bind (body status headers)
          (dex:get (localhost))
        (is status 200)
        (is (gethash "content-type" headers) "text/plain; charset=utf-8")
        (like body "This is a text for test."))))

(define-app-test |binary file|
  (lambda (env)
    @ignore env
    (let ((file (merge-pathnames #p"tmp/redhat.png" *clack-pathname*)))
      `(200
        (:content-type "image/png"
         :content-length ,(file-size file))
        ,file)))
  (lambda ()
    (multiple-value-bind (body status headers)
        (dex:get (localhost "redhat.png"))
      (is status 200)
      (is (gethash "content-type" headers) "image/png")
      (if (eq *clack-test-handler* :wookie)
          (is (gethash "transfer-encoding" headers) "chunked"
            "Wookie always returns with Transfer-Encoding: chunked and no Content-Length.")
          (ok (gethash "content-length" headers)))
      (is (length body) 12155))))

(define-app-test |bigger file|
  (lambda (env)
    @ignore env
    (let ((file (merge-pathnames #p"tmp/jellyfish.jpg" *clack-pathname*)))
      `(200
        (:content-type "image/jpeg"
         :content-length ,(file-size file))
        ,file)))
  (lambda ()
    (multiple-value-bind (body status headers)
        (dex:get (localhost "jellyfish.jpg"))
      (is status 200)
      (is (gethash "content-type" headers) "image/jpeg")
      (if (eq *clack-test-handler* :wookie)
          (is (gethash "transfer-encoding" headers) "chunked"
            "Wookie always returns with Transfer-Encoding: chunked and no Content-Length.")
          (ok (gethash "content-length" headers)))
      (is (length body) 139616))))

(define-app-test |handle HTTP-Header|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(gethash "foo" (getf env :headers)))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (dex:get (localhost "foo/?ediweitz=weitzedi")
                 :headers '(("Foo" . "Bar")))
      (is status 200)
      (is (gethash "content-type" headers) "text/plain; charset=utf-8")
      (is body "Bar"))))

(define-app-test |handler HTTP-Cookie|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(gethash "cookie" (getf env :headers)))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (dex:get (localhost "foo/?ediweitz=weitzedi")
                 :headers '(("Cookie" . "foo")))
      (is status 200)
      (is (gethash "content-type" headers) "text/plain; charset=utf-8")
      (is body "foo"))))

(define-app-test |validate env|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(with-output-to-string (str)
          (loop for h in '(:request-method
                           :path-info
                           :query-string
                           :server-name
                           :server-port)
                do (format str "~A:~S~%" h (getf env h)))))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (dex:get (localhost "foo/?ediweitz=weitzedi"))
      (is status 200)
      (is (gethash "content-type" headers) "text/plain; charset=utf-8")
      (is body (format nil "~{~A~%~}"
                       `("REQUEST-METHOD::GET"
                         "PATH-INFO:\"/foo/\""
                         "QUERY-STRING:\"ediweitz=weitzedi\""
                         "SERVER-NAME:\"localhost\""
                         ,(format nil "SERVER-PORT:~D" *clack-test-access-port*)))))))

(define-app-test |validate env (must be integer)|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(with-output-to-string (str)
          (loop for h in '(:server-port
                           :remote-port
                           :content-length)
                do (format str "~A:~A~%" h (typep (getf env h) '(or integer null))))))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (dex:post (localhost)
                  :content '(("name" . "eitaro")))
      (is status 200)
      (is (gethash "content-type" headers) "text/plain; charset=utf-8")
      (is body (format nil "~{~A~%~}"
                       `("SERVER-PORT:T"
                         "REMOTE-PORT:T"
                         "CONTENT-LENGTH:T"))))))

(define-app-test |% encoding in PATH-INFO|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(getf env :path-info))))
  (lambda ()
    (is (dex:get (localhost "foo/bar%2cbaz")) "/foo/bar,baz")))

(define-app-test |% double encoding in PATH-INFO|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(getf env :path-info))))
  (lambda ()
    (is (dex:get (localhost "foo/bar%252cbaz")) "/foo/bar%2cbaz")))

(define-app-test |% encoding in PATH-INFO (outside of URI characters)|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(getf env :path-info))))
  (lambda ()
    (is (dex:get (localhost "foo%E3%81%82"))
        (format nil "/foo~A"
                (flex:octets-to-string #(#xE3 #x81 #x82) :external-format :utf-8)))))

(define-app-test |SERVER-PROTOCOL is required|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(prin1-to-string (getf env :server-protocol)))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (dex:get (localhost "foo/?ediweitz=weitzedi"))
      (is status 200)
      (is (gethash "content-type" headers) "text/plain; charset=utf-8")
      (like body "^:HTTP/1\\.[01]$"))))

(define-app-test |SCRIPT-NAME should not be nil|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(princ-to-string (not (null (getf env :script-name)))))))
  (lambda ()
    (is (dex:get (localhost "foo/?ediweitz=weitzedi"))
        "T"
        :test #'equalp)))

(define-app-test |Do not crash when the app dies|
  (lambda (env)
    @ignore env
    (error "Throwing an exception from app handler. Server shouldn't crash."))
  (lambda ()
    (is (nth-value 1 (dex:get (localhost)))
        500))
  nil)

(define-app-test |multi headers (request)|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(gethash "foo" (getf env :headers)))))
  (lambda ()
    (like
     (dex:get (localhost)
              :headers '(("Foo" . "bar")
                         ("Foo" . "baz")))
     "^bar,\\s*baz$")))

(define-app-test |multi headers (response)|
  (lambda (env)
    @ignore env
    `(200
      (:content-type "text/plain; charset=utf-8"
       :x-foo "foo"
       :x-foo "bar, baz")
      ("hi")))
  (lambda ()
    (let ((headers (nth-value 2 (dex:get (localhost)))))
      (like (gethash "x-foo" headers) "foo,\\s*bar,\\s*baz"))))

(define-app-test |multi Set-Cookie headers (response)|
  (lambda (env)
    @ignore env
    `(200
      (:content-type "text/plain; charset=utf-8"
       :set-cookie "SID=8ac23780325277ed40b2a382a4b02094ad0f0e12"
       :set-cookie "name=guest")
      ("hi")))
  (lambda ()
    (let ((headers (nth-value 2 (dex:get (localhost)))))
      (is (gethash "set-cookie" headers) '("SID=8ac23780325277ed40b2a382a4b02094ad0f0e12" "name=guest")))))

(define-app-test |Do not set COOKIE|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8"
       :x-cookie ,(not (null (getf env :cookie))))
      (,(gethash "cookie" (getf env :headers)))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (dex:get (localhost)
                 :headers '(("Cookie" . "foo=bar")))
      (is status 200)
      (is (gethash "x-cookie" headers) nil)
      (is body "foo=bar"))))

;; NOTE: This may fail on Hunchentoot because of its bug.
;;   Hunchentoot returns Content-Type header
;;   though 304 Not Modified.
;; And Wookie also always returns Transfer-Encoding header.
(define-app-test |no entity headers on 304|
  (lambda (env)
    @ignore env
    `(304 nil nil))
  (lambda ()
    (if (or (eq *clack-test-handler* :hunchentoot)
            (eq *clack-test-handler* :toot)
            (eq *clack-test-handler* :wookie))
        (skip 5 (format nil "because of ~:(~A~)'s bug" *clack-test-handler*))
        (multiple-value-bind (body status headers)
            (dex:get (localhost))
          (is status 304)
          (is body #() :test #'equalp)
          (is (nth-value 1 (gethash "content-type" headers)) nil "No Content-Type")
          (is (nth-value 1 (gethash "content-length" headers)) nil "No Content-Length")
          (is (nth-value 1 (gethash "transfer-encoding" headers)) nil "No Transfer-Encoding")))))

(define-app-test |REQUEST-URI is set|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(getf env :request-uri))))
  (lambda ()
    (if (eq *clack-test-handler* :toot)
        (skip 1 "because of ~:(~A~)'s bug" *clack-test-handler*)
        (is (dex:get (localhost "foo/bar%20baz%73?x=a")) "/foo/bar%20baz%73?x=a"))))

(define-app-test |a big header value > 128 bytes|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(gethash "x-foo" (getf env :headers)))))
  (lambda ()
    (let ((chunk
           (with-output-to-string (chunk)
             (dotimes (i 12000) (write-string "abcdefgh" chunk))
             chunk)))
      (multiple-value-bind (body status)
          (dex:get (localhost)
                   :headers `(("X-Foo" . ,chunk)))
        (if (eq :fcgi *clack-test-handler*)
            (progn
              (is status 400)
              (like body "400 Request Header Or Cookie Too Large"))
            (progn
              (is status 200)
              (is body chunk)))))))

(define-app-test |CRLF output|
  (lambda (env)
    @ignore env
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(format nil "Foo: Bar~A~A~A~AHello World"
                #\Return #\NewLine #\Return #\NewLine))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (dex:get (localhost))
      (is status 200)
      (is (gethash "foo" headers) nil)
      (is body (format nil "Foo: Bar~A~A~A~AHello World"
                       #\Return #\NewLine #\Return #\NewLine)))))

(define-app-test |test 404|
  (lambda (env)
    @ignore env
    '(404
      (:content-type "text/plain; charset=utf-8")
      ("Not Found")))
  (lambda ()
    (multiple-value-bind (body status)
        (dex:get (localhost))
      (is status 404)
      (is body "Not Found"))))

(define-app-test |request -> input seekable|
  (lambda (env)
    (let ((body (make-array 4 :element-type '(unsigned-byte 8))))
      (read-sequence body (getf env :raw-body))
      `(200
        (:content-type "text/plain; charset=utf-8")
        (,(babel:octets-to-string body)))))
  (lambda ()
    (is (dex:post (localhost)
                  :content "body")
        "body")))

(define-app-test |Content-Length 0 is not set Transfer-Encoding|
  (lambda (env)
    @ignore env
    `(200
      (:content-length 0)
      ("")))
  (lambda ()
    (multiple-value-bind (body status headers)
        (dex:get (localhost))
      (is status 200)
      (is (gethash "client-transfer-encoding" headers) nil)
      (is body #() :test #'equalp))))

(define-app-test |handle Authorization header|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8"
       :x-authorization ,(not (null (gethash "authorization" (getf env :headers)))))
      (,(gethash "authorization" (getf env :headers) ""))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (dex:get (localhost)
                 :headers '(("Authorization" . "Basic XXXX")))
      (is status 200)
      (is (gethash "x-authorization" headers) "T"
          :test #'equalp)
      (is body "Basic XXXX"))
    ;; XXX: On Wookie handler, this raises USOCKET:CONNECTION-REFUSED-ERROR.
    (unless (eq *clack-test-handler* :wookie)
      (multiple-value-bind (body status headers)
          (dex:get (localhost))
        (is status 200)
        (is (gethash "x-authorization" headers) nil)
        (ok (member body '(nil "") :test #'equal))))))

(define-app-test |repeated slashes|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(getf env :path-info))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (dex:get (localhost "foo///bar/baz"))
      (is status 200)
      (is (gethash "content-type" headers) "text/plain; charset=utf-8")
      (is body "/foo///bar/baz"))))

(define-app-test |file upload|
  (lambda (env)
    (destructuring-bind (name body params headers)
        (car (http-body:parse
              (getf env :content-type)
              (getf env :content-length)
              (getf env :raw-body)))
      (declare (ignore name body headers))
      `(200
        (:content-type "text/plain; charset=utf-8")
        (,(gethash "filename" params)))))
  (lambda ()
    (multiple-value-bind (body status)
        (dex:post (localhost)
                  :content
                  `(("file" . ,(merge-pathnames #p"tmp/file.txt" *clack-pathname*))))
      (is status 200)
      (is body "file.txt"))))

(define-app-test |streaming|
  (lambda (env)
    (declare (ignore env))
    (lambda (res)
      (let ((writer (funcall res '(200 (:content-type "text/plain")))))
        (loop for i from 0 to 2
              do (sleep 1)
                 (funcall writer (format nil "~S~%" i)))
        (funcall writer "" :close t))))
  (lambda ()
    (if (find *clack-test-handler* '(:hunchentoot
                                     :toot
                                     :fcgi
                                     :wookie
                                     :woo))
        (multiple-value-bind (body status)
            (dex:get (localhost))
          (is status 200)
          (is body (format nil "0~%1~%2~%")))
        (skip 2 (format nil "because ~:(~A~) doesn't support streaming" *clack-test-handler*)))))

(doc:start)

@doc:NAME "
Clack.Test.Suite - Test suite for Clack handlers.
"

@doc:SYNOPSIS "
    ;; Tests Clack.Handler.Hunchentoot.
    (clack.test.suite:run-server-tests :hunchentoot)
    
    ;; Tests one test.
    (clack.test.suite:run-server-tests :hunchentoot '|% double encoding in PATH-INFO|)
"

@doc:DESCRIPTION "
Clack.Test.Suite is a test suite to test a new Clack server implementation. It automatically loads a new handler environment and uses Dexador to send HTTP requests to the local server to make sure your handler implements the Clack specification correctly.

Your Lisp have to support multi-thread to run these tests.
"

@doc:AUTHOR "
Eitaro Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Test
"
