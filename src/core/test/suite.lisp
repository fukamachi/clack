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
        :cl-test-more)
  (:export :run-server-tests))

(in-package :clack.test.suite)

(defvar *handler-package* nil
  "Handler package to test.")
(defvar *tests* '()
  "Collection of tests for Clack Handler.")
(defvar *clack-pathname*
    (asdf:component-pathname (asdf:find-system :clack)))

(defun run-server-tests (handler-name)
  "Run tests for clack.handler.
Handler name is a keyword and doesn't include the clack.handler prefix.
For example, if you have a handler `clack.handler.foo',
you would call like this: `(run-server-tests :foo)'."
  (setq *drakma-default-external-format* :utf-8)
  (setf *handler-package*
        (find-package
         (concatenate 'string "CLACK.HANDLER."
                      (symbol-name handler-name))))
  (plan (length *tests*))
  (dolist (test *tests*)
    (apply #'test test))
  (finalize))

(defun test (desc fn app)
  "Test each registed tests."
  (let ((acceptor (funcall (intern "RUN" *handler-package*)
                           app :port 4242 :debug t)))
    (when desc (diag desc))
    (unwind-protect
        (funcall fn)
      (funcall (intern "STOP" *handler-package*) acceptor))))

(defmacro deftest (desc fn app)
  "Regist a test. Note that `desc' should be a string."
  (let ((test (gensym "TEST")))
    `(let ((,test (list ,desc ,fn ,app)))
       (sif (member ,desc *tests*
                    :key #'car
                    :test #'string=)
            (rplaca it ,test)
            (push ,test *tests*)))))

(defun get-header (headers key)
  (cdr (assoc key headers)))

(defun file-size (file)
  (with-open-file (in file :direction :input)
    (file-length in)))

;; Tests

(deftest "SCRIPT-NAME"
  (lambda ()
    (is (http-request "http://localhost:4242/") nil))
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :script-name)))))

(deftest "GET"
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/?name=fukamachi")
      (is status 200)
      (is (get-header headers :content-type)
          "text/plain")
      (is body "Hello, name=fukamachi")))
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(format nil "Hello, ~A" (getf req :query-string))))))

(deftest "POST"
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/"
                      :method :post
                      :parameters '(("name" . "eitarow")))
      (is status 200)
      (is (get-header headers :client-content-length) "12")
      (is (get-header headers :client-content-type) "application/x-www-form-urlencoded")
      (is body "Hello, name=eitarow")))
  (lambda (req)
    (let ((body (read-line (getf req :raw-body))))
      `(200
        (:content-type "text/plain"
         :client-content-length ,(getf req :content-length)
         :client-content-type ,(getf req :content-type))
        (,(format nil "Hello, ~A" body))))))

(deftest "big POST"
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
        (is (length body) len))))
  (lambda (req)
    (let ((body
           (make-array (getf req :content-length) :element-type 'octet)))
      (read-sequence body (getf req :raw-body))
      `(200
        (:content-type "text/plain"
         :client-content-length ,(getf req :content-length)
         :client-content-type ,(getf req :content-type))
        (,(octets-to-string body))))))

(deftest "url-scheme"
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/" :method :post)
      (is status 200)
      (is (get-header headers :content-type) "text/plain")
      (is body "HTTP")))
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :url-scheme)))))

(deftest "return pathname"
  (lambda ()
      (multiple-value-bind (body status headers)
          (http-request "http://localhost:4242/")
        (is status 200)
        (is (get-header headers :content-type) "text/plain")
        (like body "This is a text for test.")))
  (lambda (req)
    (declare (ignore req))
    `(200
      (:content-type "text/plain")
      ,(merge-pathnames #p"tmp/file.txt" *clack-pathname*))))

(deftest "binary file"
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/redhat.png")
      (is status 200)
      (is (get-header headers :content-type) "image/png")
      (is (length body) 12155)))
  (lambda (req)
    (declare (ignore req))
    (let ((file (merge-pathnames #p"tmp/redhat.png" *clack-pathname*)))
      `(200
        (:content-type "image/png"
         :content-length ,(file-size file))
        ,file))))

(deftest "bigger file"
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/jellyfish.jpg")
      (is status 200)
      (is (get-header headers :content-type) "image/jpeg")
      (is (length body) 139616)))
  (lambda (req)
    (declare (ignore req))
    (let ((file (merge-pathnames #p"tmp/jellyfish.jpg" *clack-pathname*)))
      `(200
        (:content-type "image/jpeg"
         :content-length ,(file-size file))
        ,file))))

(deftest "handle HTTP-Header"
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/foo/?ediweitz=weitzedi"
                      :additional-headers '(("Foo" . "Bar")))
      (is status 200)
      (is (get-header headers :content-type) "text/plain")
      (is body "Bar")))
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :http-foo)))))

(deftest "handler HTTP-Cookie"
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/foo/?ediweitz=weitzedi"
                      :additional-headers '(("Cookie" . "foo")))
      (is status 200)
      (is (get-header headers :content-type) "text/plain")
      (is body "foo")))
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :http-cookie)))))

(deftest "validate req"
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
                         "SERVER-PORT:4242")))))
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(apply #'concatenate 'string
               (loop for h in '(:request-method
                                :path-info
                                :query-string
                                :server-name
                                :server-port)
                     collect (format nil "~A:~A~%" h (getf req h))))))))

(deftest "% encoding in PATH-INFO"
  (lambda ()
    (is (http-request "http://localhost:4242/foo/bar%2cbaz") "/foo/bar,baz"))
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :path-info)))))

(deftest "% double encoding in PATH-INFO"
  (lambda ()
    (is (http-request "http://localhost:4242/foo/bar%252cbaz") "/foo/bar%2cbaz"))
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :path-info)))))

(deftest "% encoding in PATH-INFO (outside of URI characters)"
  (lambda ()
    (is (http-request "http://localhost:4242/foo%E3%81%82")
        (format nil "/foo~A"
                (flex:octets-to-string #(#xE3 #x81 #x82) :external-format :utf-8))))
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :path-info)))))

(deftest "SERVER-PROTOCOL is required"
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/foo/?ediweitz=weitzedi")
      (is status 200)
      (is (get-header headers :content-type) "text/plain")
      (like body "^HTTP/1\\.[01]$")))
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :server-protocol)))))

(deftest "SCRIPT-NAME should not be nil"
  (lambda ()
    (is (http-request "http://localhost:4242/foo/?ediweitz=weitzedi")
        "T"
        :test #'equalp))
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(not (null (getf req :script-name)))))))

(deftest "Do not crash when the app dies"
  (lambda ()
    (is (nth-value 1 (http-request "http://localhost:4242/"))
        500))
  (lambda (req)
    (declare (ignore req))
    (error "Throwing an exception from app handler. Server shouldn't crash.")))

(deftest "multi headers (request)"
  (lambda ()
    (like
     (http-request "http://localhost:4242/"
                   :additional-headers '(("Foo" . "bar")
                                         ("Foo" . "baz")))
     "^bar,\\s*baz$"))
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :http-foo)))))

(deftest "multi headers (response)"
  (lambda ()
    (let ((headers (nth-value 2 (http-request "http://localhost:4242/"))))
      (like (get-header headers :x-foo) "foo,\\s*bar,\\s*baz")))
  (lambda (req)
    (declare (ignore req))
    `(200
      (:content-type "text/plain"
       :x-foo "foo"
       :x-foo "bar, baz")
      ("hi"))))

(deftest "Do not set COOKIE"
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/"
                      :additional-headers '(("Cookie" . "foo=bar")))
      (is status 200)
      (is (get-header headers :x-cookie) nil)
      (is body "foo=bar")))
  (lambda (req)
    `(200
      (:content-type "text/plain"
       :x-cookie ,(not (null (getf req :cookie))))
      (,(getf req :http-cookie)))))

(deftest "no entity headers on 304"
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/")
      (is status 304)
      (is body nil)
      (ok (not (get-header headers :content-type)) "No Content-Type")
      (ok (not (get-header headers :content-length)) "No Content-Length")
      (ok (not (get-header headers :transfer-encoding)) "No Transfer-Encoding")))
  (lambda (req)
    (declare (ignore req))
    `(304 nil nil)))

(deftest "REQUEST-URI is set"
  (lambda ()
    (is (http-request "http://localhost:4242/foo/bar%20baz%73?x=a")
        "/foo/bar%20baz%73?x=a"))
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :request-uri)))))

(deftest "a big header value > 128 bytes"
  (lambda ()
    (let ((chunk
           (with-output-to-string (chunk)
             (dotimes (i 12000) (write-string "abcdefgh" chunk))
             chunk)))
      (multiple-value-bind (body status)
          (http-request "http://localhost:4242/"
                        :additional-headers `(("X-Foo" . ,chunk)))
        (is status 200)
        (is body chunk))))
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :http-x-foo)))))

(deftest "CRLF output"
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/")
      (is status 200)
      (is (get-header headers :foo) nil)
      (is body (format nil "Foo: Bar~A~A~A~AHello World"
                       #\Return #\NewLine #\Return #\NewLine))))
  (lambda (req)
    (declare (ignore req))
    `(200
      (:content-type "text/plain")
      (,(format nil "Foo: Bar~A~A~A~AHello World"
                #\Return #\NewLine #\Return #\NewLine)))))

(deftest "test 404"
  (lambda ()
    (multiple-value-bind (body status)
        (http-request "http://localhost:4242/")
      (is status 404)
      (is body "Not Found")))
  (lambda (req)
    (declare (ignore req))
    `(404
      (:content-type "text/plain")
      ("Not Found"))))

(deftest "request -> input seekable"
  (lambda ()
    (is (http-request "http://localhost:4242/"
                      :method :post
                      :parameters '(("body" . nil)))
        "body"))
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(read-line (getf req :raw-body))))))

(deftest "Content-Length 0 is not set Transfer-Encoding"
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/")
      (is status 200)
      (is (get-header headers :client-transfer-encoding) nil)
      (is body nil)))
  (lambda (req)
    (declare (ignore req))
    `(200
      (:content-length 0)
      (""))))

(deftest "handle Authorization header"
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/")
      (is status 200)
      (is (get-header headers :x-authorization) t)
      (is body "Basic XXXX"))
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/")
      (is status 200)
      (is (get-header headers :x-authorization) nil)
      (is body "")))
  (lambda (req)
    `(200
      (:content-type "text/plain"
       :x-authorization ,(not (null (getf req :http-authorization))))
      (,(or (getf req :http-authorization) "")))))

(deftest "repeated slashes"
  (lambda ()
    (multiple-value-bind (body status headers)
        (http-request "http://localhost:4242/foo///bar/baz")
      (is status 200)
      (is (get-header headers :content-type) "text/plain")
      (is body "/foo///bar/baz")))
  (lambda (req)
    `(200
      (:content-type "text/plain")
      (,(getf req :path-info)))))
