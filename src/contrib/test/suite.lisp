#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.test.suite
  (:use :cl
        :anaphora
        :cl-test-more)
  (:import-from :flexi-streams
                :octet
                :octets-to-string)
  (:import-from :clack.test
                :*clack-test-handler*
                :*clack-test-port*
                :define-app-test)
  (:import-from :drakma
                :*drakma-default-external-format*
                :http-request)
  (:import-from :asdf
                :find-system
                :component-pathname))

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
  (let ((*drakma-default-external-format* :utf-8)
        (*clack-test-handler* handler-name))
    (if name
        (run-test name)
        (progn
          (plan 29)
          (run-test-package :clack.test.suite)))))

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

(define-app-test |SCRIPT-NAME|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(getf env :script-name))))
  (lambda ()
    (ok (member (http-request (localhost)) '(nil "") :test #'equal))))

(define-app-test |GET|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(format nil "Hello, ~A" (getf env :query-string)))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request (localhost "?name=fukamachi"))
      (is status 200)
      (is (get-header headers :content-type)
          "text/plain; charset=utf-8")
      (is body "Hello, name=fukamachi"))))

(define-app-test |POST|
  (lambda (env)
    (let ((body (read-line (getf env :raw-body))))
      `(200
        (:content-type "text/plain; charset=utf-8"
         :client-content-length ,(getf env :content-length)
         :client-content-type ,(getf env :content-type))
        (,(format nil "Hello, ~A" body)))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request (localhost)
                      :method :post
                      :parameters '(("name" . "eitarow")))
      (is status 200)
      (is (get-header headers :client-content-length) "12")
      (is (get-header headers :client-content-type) "application/x-www-form-urlencoded")
      (is body "Hello, name=eitarow"))))

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
          (http-request (localhost)
                        :method :post
                        :content-type "application/octet-stream"
                        :content-length len
                        :parameters `((,chunk)))
        (is status 200)
        (is (get-header headers :client-content-length)
            (princ-to-string len))
        (is (length body) len)))))

(define-app-test |url-scheme|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(getf env :url-scheme))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request (localhost) :method :post)
      (is status 200)
      (is (get-header headers :content-type) "text/plain; charset=utf-8")
      (is body "HTTP"))))

(define-app-test |return pathname|
  (lambda (env)
    @ignore env
    `(200
      (:content-type "text/plain; charset=utf-8")
      ,(merge-pathnames #p"tmp/file.txt" *clack-pathname*)))
  (lambda ()
      (multiple-value-bind (body status headers)
          (http-request (localhost))
        (is status 200)
        (is (get-header headers :content-type) "text/plain; charset=utf-8")
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
        (http-request (localhost "redhat.png"))
      (is status 200)
      (is (get-header headers :content-type) "image/png")
      (ok (get-header headers :content-length))
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
        (http-request (localhost "jellyfish.jpg"))
      (is status 200)
      (is (get-header headers :content-type) "image/jpeg")
      (ok (get-header headers :content-length))
      (is (length body) 139616))))

(define-app-test |handle HTTP-Header|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(getf env :http-foo))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request (localhost "foo/?ediweitz=weitzedi")
                      :additional-headers '(("Foo" . "Bar")))
      (is status 200)
      (is (get-header headers :content-type) "text/plain; charset=utf-8")
      (is body "Bar"))))

(define-app-test |handler HTTP-Cookie|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(getf env :http-cookie))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request (localhost "foo/?ediweitz=weitzedi")
                      :additional-headers '(("Cookie" . "foo")))
      (is status 200)
      (is (get-header headers :content-type) "text/plain; charset=utf-8")
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
        (http-request (localhost "foo/?ediweitz=weitzedi"))
      (is status 200)
      (is (get-header headers :content-type) "text/plain; charset=utf-8")
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
                do (format str "~A:~A~%" h (typep (getf env h) 'integer)))))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request (localhost)
                      :method :post
                      :parameters '(("name" . "eitarow")))
      (is status 200)
      (is (get-header headers :content-type) "text/plain; charset=utf-8")
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
    (is (http-request (localhost "foo/bar%2cbaz")) "/foo/bar,baz")))

(define-app-test |% double encoding in PATH-INFO|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(getf env :path-info))))
  (lambda ()
    (is (http-request (localhost "foo/bar%252cbaz")) "/foo/bar%2cbaz")))

(define-app-test |% encoding in PATH-INFO (outside of URI characters)|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(getf env :path-info))))
  (lambda ()
    ;; XXX: Though URI must not include non-ASCII chars,
    ;;      PURI decodes PATH automatically.
    (let ((uri (puri:parse-uri (localhost "foo%E3%81%82"))))
      (setf (slot-value uri 'puri::path) "/foo%E3%81%82")
      (is (http-request uri)
          (format nil "/foo~A"
                  (flex:octets-to-string #(#xE3 #x81 #x82) :external-format :utf-8))))))

(define-app-test |SERVER-PROTOCOL is required|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(prin1-to-string (getf env :server-protocol)))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request (localhost "foo/?ediweitz=weitzedi"))
      (is status 200)
      (is (get-header headers :content-type) "text/plain; charset=utf-8")
      (like body "^:HTTP/1\\.[01]$"))))

(define-app-test |SCRIPT-NAME should not be nil|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(not (null (getf env :script-name))))))
  (lambda ()
    (is (http-request (localhost "foo/?ediweitz=weitzedi"))
        "T"
        :test #'equalp)))

(define-app-test |Do not crash when the app dies|
  (lambda (env)
    @ignore env
    (error "Throwing an exception from app handler. Server shouldn't crash."))
  (lambda ()
    (is (nth-value 1 (http-request (localhost)))
        500))
  nil)

(define-app-test |multi headers (request)|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(getf env :http-foo))))
  (lambda ()
    (like
     (http-request (localhost)
                   :additional-headers '(("Foo" . "bar")
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
    (let ((headers (nth-value 2 (http-request (localhost)))))
      (like (get-header headers :x-foo) "foo,\\s*bar,\\s*baz"))))

(define-app-test |Do not set COOKIE|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8"
       :x-cookie ,(not (null (getf env :cookie))))
      (,(getf env :http-cookie))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request (localhost)
                      :additional-headers '(("Cookie" . "foo=bar")))
      (is status 200)
      (is (get-header headers :x-cookie) nil)
      (is body "foo=bar"))))

;; NOTE: This may fail on Hunchentoot because it's bug.
;;   Hunchentoot returns Content-Type and Content-Length headers
;;   though 304 Not Modified.
(define-app-test |no entity headers on 304|
  (lambda (env)
    @ignore env
    `(304 nil nil))
  (lambda ()
    (if (eq :hunchentoot *clack-test-handler*)
        (skip 5 "because of Hunchentoot's bug")
        (multiple-value-bind (body status headers)
            (http-request (localhost))
          (is status 304)
          (is body #() :test #'equalp)
          (is (nth-value 1 (get-header headers :content-type)) nil "No Content-Type")
          (is (nth-value 1 (get-header headers :content-length)) nil "No Content-Length")
          (is (nth-value 1 (get-header headers :transfer-encoding)) nil "No Transfer-Encoding")))))

(define-app-test |REQUEST-URI is set|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(getf env :request-uri))))
  (lambda ()
    (let ((uri (puri:parse-uri (localhost "foo/bar%20baz%73?x=a"))))
      (setf (puri:uri-path uri) "/foo/bar%20baz%73")
      (is (http-request uri) "/foo/bar%20baz%73?x=a"))))

(define-app-test |a big header value > 128 bytes|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(getf env :http-x-foo))))
  (lambda ()
    (let ((chunk
           (with-output-to-string (chunk)
             (dotimes (i 12000) (write-string "abcdefgh" chunk))
             chunk)))
      (multiple-value-bind (body status)
          (http-request (localhost)
                        :additional-headers `(("X-Foo" . ,chunk)))
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
        (http-request (localhost))
      (is status 200)
      (is (get-header headers :foo) nil)
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
        (http-request (localhost))
      (is status 404)
      (is body "Not Found"))))

(define-app-test |request -> input seekable|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(read-line (getf env :raw-body)))))
  (lambda ()
    (is (http-request (localhost)
                      :method :post
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
        (http-request (localhost))
      (is status 200)
      (is (get-header headers :client-transfer-encoding) nil)
      (is body nil))))

(define-app-test |handle Authorization header|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8"
       :x-authorization ,(not (null (getf env :http-authorization))))
      (,(or (getf env :http-authorization) ""))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request (localhost)
                      :additional-headers '(("Authorization" . "Basic XXXX")))
      (is status 200)
      (is (get-header headers :x-authorization) "T"
          :test #'equalp)
      (is body "Basic XXXX"))
    (multiple-value-bind (body status headers)
        (http-request (localhost))
      (is status 200)
      (is (get-header headers :x-authorization) nil)
      (ok (member body '(nil "") :test #'equal)))))

(define-app-test |repeated slashes|
  (lambda (env)
    `(200
      (:content-type "text/plain; charset=utf-8")
      (,(getf env :path-info))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request (localhost "foo///bar/baz"))
      (is status 200)
      (is (get-header headers :content-type) "text/plain; charset=utf-8")
      (is body "/foo///bar/baz"))))

(cl-test-more::remove-exit-hook)

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
Clack.Test.Suite is a test suite to test a new Clack server implementation. It automatically loads a new handler environment and uses Drakma to send HTTP requests to the local server to make sure your handler implements the Clack specification correctly.

Your Lisp have to support multi-thread to run these tests.
"

@doc:AUTHOR "
Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Test
"
