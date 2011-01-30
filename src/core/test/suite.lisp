#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Test suite for Clack handlers.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)

(defpackage clack.test.suite
  (:use :cl
        :asdf
        :anaphora
        :drakma
        :flexi-streams
        :cl-test-more
        :clack.test)
  (:export :run-server-tests :run-test))

(in-package :clack.test.suite)

(defvar *clack-pathname*
    (asdf:component-pathname (asdf:find-system :clack)))

(defun run-server-tests (handler-name &optional name)
  "Run tests for clack.handler.
Handler name is a keyword and doesn't include the clack.handler prefix.
For example, if you have a handler `clack.handler.foo',
you would call like this: `(run-server-tests :foo)'."
  (setq *drakma-default-external-format* :utf-8)
  (setf *clack-test-handler*
        (concatenate 'string "CLACK.HANDLER."
                     (symbol-name handler-name)))
  (setf *clack-test-port* 4242)
  (if name
      (progn
        (plan nil)
        (run-test (intern name)))
      (progn
        (plan 71)
        (run-test-all))))

(defun get-header (headers key)
  (let ((val (assoc key headers)))
    (values (cdr val) (not (null val)))))

(defun file-size (file)
  (with-open-file (in file :direction :input)
    (file-length in)))

;; Tests

(define-app-test "SCRIPT-NAME"
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :script-name))))
  (lambda ()
    (is (http-request "http://localhost:4242/") nil)))

(define-app-test "GET"
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(format nil "Hello, ~A" (getf req :query-string)))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/?name=fukamachi")
      (is status 200)
      (is (get-header headers :content-type)
          "text/plain")
      (is body "Hello, name=fukamachi"))))

(define-app-test "POST"
  (lambda (req)
    (let ((body (read-line (getf req :raw-body))))
      `(200
        (:content-type "text/plain"
         :client-content-length ,(getf req :content-length)
         :client-content-type ,(getf req :content-type))
        (,(format nil "Hello, ~A" body)))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/"
                      :method :post
                      :parameters '(("name" . "eitarow")))
      (is status 200)
      (is (get-header headers :client-content-length) "12")
      (is (get-header headers :client-content-type) "application/x-www-form-urlencoded")
      (is body "Hello, name=eitarow"))))

(define-app-test "big POST"
  (lambda (req)
    (let ((body
           (make-array (getf req :content-length) :element-type 'octet)))
      (read-sequence body (getf req :raw-body))
      `(200
        (:content-type "text/plain"
         :client-content-length ,(getf req :content-length)
         :client-content-type ,(getf req :content-type))
        (,(octets-to-string body)))))
  (lambda ()
    (let* ((chunk
            (with-output-to-string (chunk)
              (dotimes (i 12000) (write-string "abcdefgh" chunk))
              chunk))
           (len (length chunk)))
      (multiple-value-bind (body status headers)
          (http-request "http://localhost:4242/"
                        :method :post
                        :content-type "application/octet-stream"
                        :content-length len
                        :parameters `((,chunk)))
        (is status 200)
        (is (get-header headers :client-content-length) (format nil "~A" len))
        (is (length body) len)))))

(define-app-test "url-scheme"
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :url-scheme))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/" :method :post)
      (is status 200)
      (is (get-header headers :content-type) "text/plain")
      (is body "HTTP"))))

(define-app-test "return pathname"
  (lambda (req)
    (declare (ignore req))
    `(200
      (:content-type "text/plain")
      ,(merge-pathnames #p"tmp/file.txt" *clack-pathname*)))
  (lambda ()
      (multiple-value-bind (body status headers)
          (http-request "http://localhost:4242/")
        (is status 200)
        (is (get-header headers :content-type) "text/plain")
        (like body "This is a text for test."))))

(define-app-test "binary file"
  (lambda (req)
    (declare (ignore req))
    (let ((file (merge-pathnames #p"tmp/redhat.png" *clack-pathname*)))
      `(200
        (:content-type "image/png"
         :content-length ,(file-size file))
        ,file)))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/redhat.png")
      (is status 200)
      (is (get-header headers :content-type) "image/png")
      (is (length body) 12155))))

(define-app-test "bigger file"
  (lambda (req)
    (declare (ignore req))
    (let ((file (merge-pathnames #p"tmp/jellyfish.jpg" *clack-pathname*)))
      `(200
        (:content-type "image/jpeg"
         :content-length ,(file-size file))
        ,file)))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/jellyfish.jpg")
      (is status 200)
      (is (get-header headers :content-type) "image/jpeg")
      (is (length body) 139616))))

(define-app-test "handle HTTP-Header"
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :http-foo))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/foo/?ediweitz=weitzedi"
                      :additional-headers '(("Foo" . "Bar")))
      (is status 200)
      (is (get-header headers :content-type) "text/plain")
      (is body "Bar"))))

(define-app-test "handler HTTP-Cookie"
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :http-cookie))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/foo/?ediweitz=weitzedi"
                      :additional-headers '(("Cookie" . "foo")))
      (is status 200)
      (is (get-header headers :content-type) "text/plain")
      (is body "foo"))))

(define-app-test "validate req"
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(apply #'concatenate 'string
               (loop for h in '(:request-method
                                :path-info
                                :query-string
                                :server-name
                                :server-port)
                     collect (format nil "~A:~A~%" h (getf req h)))))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/foo/?ediweitz=weitzedi")
      (is status 200)
      (is (get-header headers :content-type) "text/plain")
      (is body (format nil "~{~A~%~}"
                       '("REQUEST-METHOD:GET"
                         "PATH-INFO:/foo/"
                         "QUERY-STRING:ediweitz=weitzedi"
                         "SERVER-NAME:localhost"
                         "SERVER-PORT:4242"))))))

(define-app-test "% encoding in PATH-INFO"
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :path-info))))
  (lambda ()
    (is (http-request "http://localhost:4242/foo/bar%2cbaz") "/foo/bar,baz")))

(define-app-test "% double encoding in PATH-INFO"
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :path-info))))
  (lambda ()
    (is (http-request "http://localhost:4242/foo/bar%252cbaz") "/foo/bar%2cbaz")))

(define-app-test "% encoding in PATH-INFO (outside of URI characters)"
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :path-info))))
  (lambda ()
    (is (http-request "http://localhost:4242/foo%E3%81%82")
        (format nil "/foo~A"
                (flex:octets-to-string #(#xE3 #x81 #x82) :external-format :utf-8)))))

(define-app-test "SERVER-PROTOCOL is required"
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :server-protocol))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/foo/?ediweitz=weitzedi")
      (is status 200)
      (is (get-header headers :content-type) "text/plain")
      (like body "^HTTP/1\\.[01]$"))))

(define-app-test "SCRIPT-NAME should not be nil"
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(not (null (getf req :script-name))))))
  (lambda ()
    (is (http-request "http://localhost:4242/foo/?ediweitz=weitzedi")
        "T"
        :test #'equalp)))

(define-app-test "Do not crash when the app dies"
  (lambda (req)
    (declare (ignore req))
    (error "Throwing an exception from app handler. Server shouldn't crash."))
  (lambda ()
    (is (nth-value 1 (http-request "http://localhost:4242/"))
        500)))

(define-app-test "multi headers (request)"
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :http-foo))))
  (lambda ()
    (like
     (http-request "http://localhost:4242/"
                   :additional-headers '(("Foo" . "bar")
                                         ("Foo" . "baz")))
     "^bar,\\s*baz$")))

(define-app-test "multi headers (response)"
  (lambda (req)
    (declare (ignore req))
    `(200
      (:content-type "text/plain"
       :x-foo "foo"
       :x-foo "bar, baz")
      ("hi")))
  (lambda ()
    (let ((headers (nth-value 2 (http-request "http://localhost:4242/"))))
      (like (get-header headers :x-foo) "foo,\\s*bar,\\s*baz"))))

(define-app-test "Do not set COOKIE"
  (lambda (req)
    `(200
      (:content-type "text/plain"
       :x-cookie ,(not (null (getf req :cookie))))
      (,(getf req :http-cookie))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/"
                      :additional-headers '(("Cookie" . "foo=bar")))
      (is status 200)
      (is (get-header headers :x-cookie) nil)
      (is body "foo=bar"))))

;; NOTE: This may fail on Hunchentoot because it's bug.
;;   Hunchentoot returns Content-Type and Content-Length headers
;;   though 304 Not Modified.
(define-app-test "no entity headers on 304"
  (lambda (req)
    (declare (ignore req))
    `(304 nil nil))
  (lambda ()
    (if (string= "CLACK.HANDLER.HUNCHENTOOT" *clack-test-handler*)
        (skip 5 "because of Hunchentoot's bug")
        (multiple-value-bind (body status headers)
            (http-request "http://localhost:4242/")
          (is status 304)
          (is body nil)
          (is (nth-value 1 (get-header headers :content-type)) nil "No Content-Type")
          (is (nth-value 1 (get-header headers :content-length)) nil "No Content-Length")
          (is (nth-value 1 (get-header headers :transfer-encoding)) nil "No Transfer-Encoding")))))

(define-app-test "REQUEST-URI is set"
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :request-uri))))
  (lambda ()
    (let ((uri (puri:parse-uri "http://localhost:4242/foo/bar%20baz%73?x=a")))
      (setf (puri:uri-path uri) "/foo/bar%20baz%73")
      (is (http-request uri) "/foo/bar%20baz%73?x=a"))))

(define-app-test "a big header value > 128 bytes"
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :http-x-foo))))
  (lambda ()
    (let ((chunk
           (with-output-to-string (chunk)
             (dotimes (i 12000) (write-string "abcdefgh" chunk))
             chunk)))
      (multiple-value-bind (body status)
          (http-request "http://localhost:4242/"
                        :additional-headers `(("X-Foo" . ,chunk)))
        (is status 200)
        (is body chunk)))))

(define-app-test "CRLF output"
  (lambda (req)
    (declare (ignore req))
    `(200
      (:content-type "text/plain")
      (,(format nil "Foo: Bar~A~A~A~AHello World"
                #\Return #\NewLine #\Return #\NewLine))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/")
      (is status 200)
      (is (get-header headers :foo) nil)
      (is body (format nil "Foo: Bar~A~A~A~AHello World"
                       #\Return #\NewLine #\Return #\NewLine)))))

(define-app-test "test 404"
  (lambda (req)
    (declare (ignore req))
    `(404
      (:content-type "text/plain")
      ("Not Found")))
  (lambda ()
    (multiple-value-bind (body status)
        (http-request "http://localhost:4242/")
      (is status 404)
      (is body "Not Found"))))

(define-app-test "request -> input seekable"
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(read-line (getf req :raw-body)))))
  (lambda ()
    (is (http-request "http://localhost:4242/"
                      :method :post
                      :content "body")
        "body")))

(define-app-test "Content-Length 0 is not set Transfer-Encoding"
  (lambda (req)
    (declare (ignore req))
    `(200
      (:content-length 0)
      ("")))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/")
      (is status 200)
      (is (get-header headers :client-transfer-encoding) nil)
      (is body nil))))

(define-app-test "handle Authorization header"
  (lambda (req)
    `(200
      (:content-type "text/plain"
       :x-authorization ,(not (null (getf req :http-authorization))))
      (,(or (getf req :http-authorization) ""))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/"
                      :additional-headers '(("Authorization" . "Basic XXXX")))
      (is status 200)
      (is (get-header headers :x-authorization) "T"
          :test #'equalp)
      (is body "Basic XXXX"))
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/")
      (is status 200)
      (is (get-header headers :x-authorization) nil)
      (is body nil :test #'eq))))

(define-app-test "repeated slashes"
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :path-info))))
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/foo///bar/baz")
      (is status 200)
      (is (get-header headers :content-type) "text/plain")
      (is body "/foo///bar/baz"))))
