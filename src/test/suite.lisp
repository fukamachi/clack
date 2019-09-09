(in-package :cl-user)
(defpackage clack.test.suite
  (:use :cl
        :rove)
  (:import-from :clack.test
                :*clack-test-handler*
                :*clack-test-port*
                :*clack-test-access-port*
                :*enable-debug*
                :testing-app)
  (:import-from :flexi-streams
                :octet
                :octets-to-string)
  (:import-from :http-body
                :parse)
  (:import-from :cl-ppcre
                :scan)
  (:export :run-server-tests))
(in-package :clack.test.suite)

(defvar *clack-pathname*
  (asdf:system-source-directory :clack))

(defun localhost (&optional (path "/"))
  (clack.test:localhost path *clack-test-access-port*))

(defun run-server-tests (handler-name)
  "Run tests for clack.handler.
Handler name is a keyword and doesn't include the clack.handler prefix.
For example, if you have a handler `clack.handler.foo',
you would call like this: `(run-server-tests :foo)'."
  (let ((*clack-test-handler* handler-name)
        (*package* (find-package :clack.test.suite))
        (dex:*use-connection-pool* nil))
    #+thread-support
    (rove:run-suite :clack.test.suite)
    #-thread-support
    (skip "Handler tests because your Lisp doesn't support threads")))

(defun get-header (headers key)
  (gethash (string-downcase key) headers))

(defun file-size (file)
  (with-open-file (in file :direction :input)
    (file-length in)))


;; Tests

(deftest response-tests
  (testing-app "list"
    (lambda (env)
      (declare (ignore env))
      '(200 (:content-type "text/plain") ("Hello" "World")))
    (multiple-value-bind (body status)
        (dex:get (localhost))
      (ok (eql status 200))
      (ok (equal body "HelloWorld"))))

  (testing-app "pathname (plain/text)"
      (lambda (env)
        (declare (ignore env))
        `(200
          (:content-type "text/plain; charset=utf-8")
          ,(merge-pathnames #p"tmp/file.txt" *clack-pathname*)))
    (multiple-value-bind (body status headers)
        (dex:get (localhost))
      (ok (eql status 200))
      (ok (equal (get-header headers :content-type) "text/plain; charset=utf-8"))
      (ok (ppcre:scan "This is a text for test." body))))

  (testing-app "pathname (binary)"
      (lambda (env)
        (declare (ignore env))
        (let ((file (merge-pathnames #p"tmp/redhat.png" *clack-pathname*)))
          `(200
            (:content-type "image/png"
             :content-length ,(file-size file))
            ,file)))
    (multiple-value-bind (body status headers)
        (dex:get (localhost "/redhat.png"))
      (ok (eql status 200))
      (ok (equal (get-header headers :content-type) "image/png"))
      (if (eq *clack-test-handler* :wookie)
          (ok (equal (get-header headers :transfer-encoding) "chunked")
              "Wookie always returns with Transfer-Encoding: chunked and no Content-Length.")
          (ok (get-header headers :content-length)))
      (ok (eql (length body) 12155))))

  (testing-app "bigger file"
      (lambda (env)
        (declare (ignore env))
        (let ((file (merge-pathnames #p"tmp/jellyfish.jpg" *clack-pathname*)))
          `(200
            (:content-type "image/jpeg"
             :content-length ,(file-size file))
            ,file)))
    (multiple-value-bind (body status headers)
        (dex:get (localhost "/jellyfish.jpg"))
      (ok (eql status 200))
      (ok (equal (get-header headers :content-type) "image/jpeg"))
      (if (eq *clack-test-handler* :wookie)
          (ok (equal (get-header headers :transfer-encoding) "chunked")
              "Wookie always returns with Transfer-Encoding: chunked and no Content-Length.")
          (ok (get-header headers :content-length)))
      (ok (eql (length body) 139616))))

  (testing-app "multi headers (response)"
      (lambda (env)
        (declare (ignore env))
        `(200
          (:content-type "text/plain; charset=utf-8"
           :x-foo "foo"
           :x-foo "bar, baz")
          ("hi")))
    (let ((headers (nth-value 2 (dex:get (localhost)))))
      (ok (ppcre:scan "foo,\\s*bar,\\s*baz" (get-header headers :x-foo)))))

  ;; NOTE: This may fail on Hunchentoot because of its bug.
  ;;   Hunchentoot returns Content-Type header
  ;;   though 304 Not Modified.
  ;; And Wookie also always returns Transfer-Encoding header.
  (testing-app "no entity headers on 304"
      (lambda (env)
        (declare (ignore env))
        `(304 nil nil))
    (if (or (eq *clack-test-handler* :hunchentoot)
            (eq *clack-test-handler* :toot)
            (eq *clack-test-handler* :wookie))
        (skip (format nil "Skipped because of ~:(~A~)'s bug" *clack-test-handler*))
        (multiple-value-bind (body status headers)
            (dex:get (localhost))
          (ok (eql status 304))
          (ok (equalp body #()))
          (ok (null (nth-value 1 (get-header headers :content-type))) "No Content-Type")
          (ok (null (nth-value 1 (get-header headers :content-length))) "No Content-Length")
          (ok (null (nth-value 1 (get-header headers :transfer-encoding))) "No Transfer-Encoding"))))

  (testing-app "CRLF output"
      (lambda (env)
        (declare (ignore env))
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(format nil "Foo: Bar~A~A~A~AHello World"
                    #\Return #\NewLine #\Return #\NewLine))))
    (multiple-value-bind (body status headers)
        (dex:get (localhost))
      (ok (eql status 200))
      (ok (null (get-header headers :foo)))
      (ok (equal body (format nil "Foo: Bar~A~A~A~AHello World"
                              #\Return #\NewLine #\Return #\NewLine)))))

  (testing-app "test 404"
      (lambda (env)
        (declare (ignore env))
        '(404
          (:content-type "text/plain; charset=utf-8")
          ("Not Found")))
    (multiple-value-bind (body status)
        (handler-bind ((dex:http-request-not-found #'dex:ignore-and-continue))
          (dex:get (localhost)))
      (ok (eql status 404))
      (ok (equal body "Not Found"))))

  (testing-app "Content-Length 0 is not set Transfer-Encoding"
      (lambda (env)
        (declare (ignore env))
        `(200
          (:content-length 0
           :content-type "text/plain")
          ("")))
    (multiple-value-bind (body status headers)
        (dex:get (localhost))
      (ok (eql status 200))
      (ok (null (get-header headers :client-transfer-encoding)))
      (ok (equal body "")))))

(deftest env-tests
  (testing-app "SCRIPT-NAME"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(getf env :script-name))))
    (ok (member (dex:get (localhost)) '(nil "") :test #'equal)))

  (testing-app "url-scheme"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(getf env :url-scheme))))
    (multiple-value-bind (body status headers)
        (dex:post (localhost))
      (ok (eql status 200))
      (ok (equal (get-header headers :content-type) "text/plain; charset=utf-8"))
      (ok (equal body "http"))))

  (testing-app "handle HTTP-Header"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(gethash "foo" (getf env :headers)))))
    (multiple-value-bind (body status headers)
        (dex:get (localhost "/foo/?ediweitz=weitzedi")
                 :headers '(("Foo" . "Bar")))
      (ok (eql status 200))
      (ok (equal (get-header headers :content-type) "text/plain; charset=utf-8"))
      (ok (equal body "Bar"))))

  (testing-app "validate env"
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
    (multiple-value-bind (body status headers)
        (dex:get (localhost "/foo/?ediweitz=weitzedi"))
      (ok (eql status 200))
      (ok (equal (get-header headers :content-type) "text/plain; charset=utf-8"))
      (ok (equal body (format nil "~{~A~%~}"
                              `("REQUEST-METHOD::GET"
                                "PATH-INFO:\"/foo/\""
                                "QUERY-STRING:\"ediweitz=weitzedi\""
                                ,(if (eq *clack-test-handler* :fcgi)
                                     "SERVER-NAME:\"localhost\"" ;; probably the name from Nginx conf
                                   "SERVER-NAME:\"127.0.0.1\"")
                                ,(format nil "SERVER-PORT:~D" *clack-test-access-port*)))))))

  (testing-app "validate env (must be integer)"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(with-output-to-string (str)
              (loop for h in '(:server-port
                               :remote-port
                               :content-length)
                    do (format str "~A:~A~%" h (typep (getf env h) '(or integer null))))))))
    (multiple-value-bind (body status headers)
        (dex:post (localhost)
                  :content '(("name" . "eitaro")))
      (ok (eql status 200))
      (ok (equal (get-header headers :content-type) "text/plain; charset=utf-8"))
      (ok (equal body (format nil "~{~{~A:~A~%~}~}"
                              `((:server-port t)
                                (:remote-port t)
                                (:content-length t)))))))

  (testing-app "% encoding in PATH-INFO"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(getf env :path-info))))
    (ok (equal (dex:get (localhost "/foo/bar%2cbaz")) "/foo/bar,baz")))

  (testing-app "% double encoding in PATH-INFO"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(getf env :path-info))))
    (ok (equal (dex:get (localhost "/foo/bar%252cbaz")) "/foo/bar%2cbaz")))

  (testing-app "% encoding in PATH-INFO (outside of URI characters)"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(getf env :path-info))))
    (ok (equal (dex:get (localhost "/foo%E3%81%82"))
               (format nil "/foo~A"
                       (flex:octets-to-string #(#xE3 #x81 #x82) :external-format :utf-8)))))

  (testing-app "Invalid UTF-8 encoded PATH-INFO"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(getf env :path-info))))
    (if (eq *clack-test-handler* :wookie)
        (skip "Skipped because do-urlencode Wookie uses cannot decode invalid UTF8 strings anyways")
        (ok (ppcre:scan (format nil "/あ~A"
                                #+abcl "\\?"
                                #-abcl #\Replacement_Character)
                        (dex:get (localhost "/%E3%81%82%BF%27%22%28"))))))

  (testing-app "SERVER-PROTOCOL is required"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(prin1-to-string (getf env :server-protocol)))))
    (multiple-value-bind (body status headers)
        (dex:get (localhost "/foo/?ediweitz=weitzedi"))
      (ok (eql status 200))
      (ok (equal (get-header headers :content-type) "text/plain; charset=utf-8"))
      (ok (ppcre:scan "^:HTTP/1\\.[01]$" body))))

  (testing-app "SCRIPT-NAME should not be nil"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(princ-to-string (not (null (getf env :script-name)))))))
    (ok (equal (dex:get (localhost "/foo/?ediweitz=weitzedi"))
               (string t))))

  (testing-app "Do not set COOKIE"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8"
           :x-cookie ,(not (null (getf env :cookie))))
          (,(gethash "cookie" (getf env :headers)))))
    (multiple-value-bind (body status headers)
        (dex:get (localhost)
                 :headers '(("Cookie" . "foo=bar")))
      (ok (eql status 200))
      (ok (null (get-header headers :x-cookie)))
      (ok (equal body "foo=bar"))))

  (testing-app "REQUEST-URI is set"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(getf env :request-uri))))
    (if (eq *clack-test-handler* :toot)
        (skip "Skipped because of Toot's bug")
        (ok (equal (dex:get (localhost "/foo/bar%20baz%73?x=a")) "/foo/bar%20baz%73?x=a")))))

(deftest request-tests
  (testing-app "GET"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(format nil "Hello, ~A" (getf env :query-string)))))
    (multiple-value-bind (body status headers)
        (dex:get (localhost "/?name=fukamachi"))
      (ok (eql status 200))
      (ok (equal (get-header headers :content-type)
                 "text/plain; charset=utf-8"))
      (ok (equal body "Hello, name=fukamachi"))))

  (testing-app "POST"
      (lambda (env)
        (let ((body (make-array 11 :element-type '(unsigned-byte 8))))
          (read-sequence body (getf env :raw-body))
          `(200
            (:content-type "text/plain; charset=utf-8"
             :client-content-length ,(getf env :content-length)
             :client-content-type ,(getf env :content-type))
            (,(format nil "Hello, ~A" (babel:octets-to-string body))))))
    (multiple-value-bind (body status headers)
        (dex:post (localhost)
                  :content '(("name" . "eitaro")))
      (ok (eql status 200))
      (ok (equal (get-header headers :client-content-length) "11"))
      (ok (equal (get-header headers :client-content-type) "application/x-www-form-urlencoded"))
      (ok (equal body "Hello, name=eitaro"))))

  (testing-app "big POST"
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
    (let* ((chunk
             (with-output-to-string (chunk)
               (dotimes (i 12000) (write-string "abcdefgh" chunk))
               chunk))
           (len (length chunk)))
      (multiple-value-bind (body status headers)
          (dex:post (localhost)
                    :headers
                    `((:content-type . "application/octet-stream")
                      (:content-length . ,len))
                    :content chunk)
        (ok (eql status 200))
        (ok (equal (get-header headers :client-content-length)
                   (princ-to-string len)))
        (ok (equal (length body) len)))))

  (testing-app "big POST (chunked)"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8"
           :client-content-length ,(getf env :content-length)
           :client-content-type ,(getf env :content-type))
          (,(let* ((body (getf env :raw-body))
                   (buffer (make-array 1024 :element-type '(unsigned-byte 8))))
              (apply #'concatenate 'string
                     (loop for read-bytes = (read-sequence buffer body)
                           collect (flex:octets-to-string (subseq buffer 0 read-bytes))
                           while (= read-bytes 1024)))))))
    (let* ((chunk
             (with-output-to-string (chunk)
               (dotimes (i 12000) (write-string "abcdefgh" chunk))
               chunk))
           (len (length chunk)))
      (multiple-value-bind (body status headers)
          (dex:post (localhost)
                    :headers '((:content-type . "application/octet-stream")
                               (:content-length . nil))
                    :content chunk)
        (ok (eql status 200))
        (if (eq *clack-test-handler* :fcgi)
            (skip "Skipped because FCGI handler always adds :CONTENT-TYPE")
            (ok (null (get-header headers :client-content-length))))
        (ok (equal (length body) len)))))

  (testing-app "multi headers (request)"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(gethash "foo" (getf env :headers)))))
    (ok (ppcre:scan
          "^bar,\\s*baz$"
          (dex:get (localhost)
                   :headers '(("Foo" . "bar")
                              ("Foo" . "baz"))))))

  (testing-app "a big header value > 128 bytes"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(gethash "x-foo" (getf env :headers)))))
    (let ((chunk
            (with-output-to-string (chunk)
              (dotimes (i 12000) (write-string "abcdefgh" chunk))
              chunk)))
      (handler-bind ((dex:http-request-failed #'dex:ignore-and-continue))
        (multiple-value-bind (body status)
            (dex:get (localhost)
                     :headers `(("X-Foo" . ,chunk)))
          (if (eq :fcgi *clack-test-handler*)
              (progn
                (ok (eql status 400))
                (ok (ppcre:scan "400 Request Header Or Cookie Too Large" body)))
              (progn
                (ok (eql status 200))
                (ok (equal body chunk))))))))

  (testing-app "request -> input seekable"
      (lambda (env)
        (let ((body (make-array 4 :element-type '(unsigned-byte 8))))
          (read-sequence body (getf env :raw-body))
          `(200
            (:content-type "text/plain; charset=utf-8")
            (,(babel:octets-to-string body)))))
    (ok (equal (dex:post (localhost)
                         :content "body")
               "body")))

  (testing-app "handle Authorization header"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8"
           :x-authorization ,(not (null (gethash "authorization" (getf env :headers)))))
          (,(gethash "authorization" (getf env :headers) ""))))
    (multiple-value-bind (body status headers)
        (dex:get (localhost)
                 :headers '(("Authorization" . "Basic XXXX")))
      (ok (eql status 200))
      (ok (equal (get-header headers :x-authorization)
                 (string t)))
      (ok (equal body "Basic XXXX")))
    ;; XXX: On Wookie handler, this raises USOCKET:CONNECTION-REFUSED-ERROR.
    (unless (eq *clack-test-handler* :wookie)
      (multiple-value-bind (body status headers)
          (dex:get (localhost))
        (ok (eql status 200))
        (ok (null (get-header headers :x-authorization)))
        (ok (member body '(nil "") :test #'equal)))))

  (testing-app "repeated slashes"
      (lambda (env)
        `(200
          (:content-type "text/plain; charset=utf-8")
          (,(getf env :path-info))))
    (multiple-value-bind (body status headers)
        (dex:get (localhost "/foo///bar/baz"))
      (ok (eql status 200))
      (ok (equal (get-header headers :content-type) "text/plain; charset=utf-8"))
      (ok (equal body "/foo///bar/baz"))))

  (testing-app "file upload"
      (lambda (env)
        (destructuring-bind (name body params headers)
            (car (http-body:parse
                  (getf env :content-type)
                  (getf env :content-length)
                  (getf env :raw-body)))
          (declare (ignore name params headers))
          `(200
            (:content-type "text/plain; charset=utf-8")
            (,(let* ((buffer (make-array 1024 :element-type '(unsigned-byte 8)))
                     (read-bytes (read-sequence buffer body)))
                (flex:octets-to-string (subseq buffer 0 read-bytes)))))))
    (multiple-value-bind (body status)
        (dex:post (localhost)
                  :content
                  `(("file" . ,(merge-pathnames #p"tmp/file.txt" *clack-pathname*))))
      (ok (eql status 200))
      (ok (equal body "This is a text for test.
"))))

  (testing-app "large file upload"
      (lambda (env)
        (destructuring-bind (name body params headers)
            (car (http-body:parse
                  (getf env :content-type)
                  (getf env :content-length)
                  (getf env :raw-body)))
          (declare (ignore name params headers))
          (let ((body-file
                  (uiop:with-temporary-file (:stream out :pathname tmp
                                             :direction :output
                                             :element-type '(unsigned-byte 8)
                                             :keep t)
                    (alexandria:copy-stream body out)
                    tmp)))
            `(200
              (:content-type "text/plain")
              (,(if (equalp (ironclad:digest-file :sha1 body-file)
                            (ironclad:digest-file :sha1 (merge-pathnames #p"tmp/jellyfish.jpg" *clack-pathname*)))
                    "ok"
                    (format nil "ng (~A)" body-file)))))))
    (multiple-value-bind (body status)
        (dex:post (localhost)
                  :content
                  `(("file" . ,(merge-pathnames #p"tmp/jellyfish.jpg" *clack-pathname*))))
      (ok (eql status 200))
      (ok (equal body "ok"))))

  (testing-app "streaming"
      (lambda (env)
        (declare (ignore env))
        (lambda (res)
          (let ((writer (funcall res '(200 (:content-type "text/plain")))))
            (loop for i from 0 to 2
                  do (sleep 1)
                     (funcall writer (format nil "~S~%" i)))
            (funcall writer "" :close t))))
    (if (find *clack-test-handler* '(:hunchentoot
                                     :toot
                                     :fcgi
                                     :wookie
                                     :woo))
        (multiple-value-bind (body status)
            (dex:get (localhost))
          (ok (eql status 200))
          (ok (equal body (format nil "0~%1~%2~%"))))
        (skip (format nil "Skipped because ~:(~A~) doesn't support streaming" *clack-test-handler*)))))

(deftest debug-tests
  (let ((*enable-debug* nil)
        (*error-output* (make-broadcast-stream)))
    (testing-app "Do not crash when the app dies"
        (lambda (env)
          (declare (ignore env))
          (error "Throwing an exception from app handler. Server shouldn't crash."))
      (handler-case (dex:get (localhost))
        (dex:http-request-internal-server-error ()
          (pass "500 Internal Server Error"))))))
