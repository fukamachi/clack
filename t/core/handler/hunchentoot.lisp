(in-package :cl-user)

(defpackage clack-test.handler.hunchentoot
  (:use :cl
        :cl-test-more
        :drakma
        :cl-ppcre
        :clack.handler.hunchentoot))

(in-package :clack-test.handler.hunchentoot)

(defparameter enable-thread-p nil)

#+(or (and allegro multiprocessing)
      armedbear
      (and cmu mp)
      scl
      corman
      (and digitool ccl-5.1)
      (and ecl threads)
      lispworks
      (and openmcl openmcl-native-threads)
      (and sbcl sb-thread)
      (and clisp mt))
(setf enable-thread-p t)
(defvar *request* nil)
(defvar *response* nil)

(if (not enable-thread-p)
    (diag "Your Lisp has no thread support. Skip Handler tests.")
    (progn
      (plan 1)
      (defvar acceptor nil)
      (defun app (req)
        (setf *request* req)
        '(200 (:content-type "text/plain") ("ok")))

      (setf acceptor (run #'app :port 4242))

      (setf *response* (http-request "http://localhost:4242/hoge/fuga?id=3"))

      (is (getf *request* :request-method) :get
          "request-method")
      (is (getf *request* :script-name) "" "script-name")
      (is (getf *request* :path-info) "/hoge/fuga"
          "path-info")
      (is (getf *request* :server-name) "localhost"
          "server-name")
      (is (getf *request* :server-port) 4242
          "server-port")
      (is (getf *request* :server-protocol) :HTTP/1.1
          "server-protocol")
      (is (getf *request* :request-uri) "/hoge/fuga?id=3"
          "request-uri")
      (is (getf *request* :url-schema) :http
          "url-schema")
      (is (ppcre:scan "^Drakma.+http://weitz\.de/drakma/\\)$"
                      (getf *request* :http-user-agent))
          0 "http-user-agent")
      (is (getf *request* :http-referer) nil
          "referer")
      (is (getf *request* :http-cookie) nil
          "cookie")
      (is (getf *request* :remote-addr) "127.0.0.1"
          "remote-addr")
      (is-type (getf *request* :remote-port) 'integer
               "remote-port")
      (is (getf *request* :query-string) "id=3"
          "query-string")
      (ok (or (null (getf *request* :raw-body))
              (typep (getf *request* :raw-body) 'stream))
          "raw-body")
      (is (getf *request* :query-parameters) '(:id "3")
          "query-parameters")
      (is (getf *request* :post-parameters) nil
          "post-parameters")

      (is *response* (format nil "ok~%") "response body")

      (stop acceptor)

      (finalize)
      ))
