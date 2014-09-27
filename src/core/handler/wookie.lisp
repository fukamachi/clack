#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.handler.wookie
  (:use :cl)
  (:import-from :clack.component
                :call)
  (:import-from :wookie
                :*state*
                :wookie-state
                :start-response
                :finish-response
                :add-hook
                :defroute
                :listener
                :ssl-listener
                :start-server
                :request-headers
                :request-resource
                :request-http
                :request-method
                :request-uri
                :request-socket)
  (:import-from :cl-async
                :with-event-loop
                :close-tcp-server
                :async-io-stream)
  (:import-from :bordeaux-threads
                :make-thread
                :destroy-thread
                :make-lock
                :acquire-lock
                :release-lock)
  (:import-from :http-parse
                :http-body
                :http-store-body
                :http-version)
  (:import-from :puri
                :uri-path
                :uri-query)
  (:import-from :do-urlencode
                :urldecode)
  (:import-from :flexi-streams
                :make-in-memory-input-stream)
  (:import-from :babel
                :string-to-octets)
  (:import-from :fast-io
                :with-fast-output
                :fast-write-sequence
                :fast-write-byte)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :alexandria
                :if-let
                :hash-table-plist
                :copy-stream))
(in-package :clack.handler.wookie)

(cl-syntax:use-syntax :annot)

;; XXX: :store-body keeps the whole POST data in-memory.
(defun parsed-headers-hook (request)
  (setf (http-parse:http-store-body (request-http request)) t))

@export
(defun run (app &key debug (port 5000)
                  ssl ssl-key-file ssl-cert-file ssl-key-password)
  (let ((server-started-lock (bt:make-lock "server-started")))
    (prog1
        (#+thread-support bt:make-thread
         #-thread-support funcall
         #'(lambda ()
             (bt:acquire-lock server-started-lock)
             (let ((*state* (make-instance 'wookie:wookie-state)))
               (add-hook :parsed-headers 'parsed-headers-hook :clack-handler-wookie-parsed-headers-hook)
               (defroute (:* ".*" :chunk nil) (req res)
                 (let ((env (handle-request req :ssl ssl)))
                   (handle-response
                    res
                    (if debug
                        (call app env)
                        (if-let (res (handler-case (call app env)
                                       (error (error)
                                         (princ error *error-output*)
                                         nil)))
                          res
                          '(500 nil nil))))))
               (log:config :error)
               (prog1
                   (as:with-event-loop ()
                     (let ((listener
                             (if ssl
                                 (make-instance 'wookie:ssl-listener
                                                :port port
                                                :key ssl-key-file
                                                :certificate ssl-cert-file
                                                :password ssl-key-password)
                                 (make-instance 'wookie:listener
                                                :port port))))
                       (start-server listener))
                     (bt:release-lock server-started-lock))
                 (log:config :info)))))
      (bt:acquire-lock server-started-lock t))))

@export
(defun stop (server)
  (#+thread-support bt:destroy-thread
   #-thread-support as:close-tcp-server
   server))

(defun handle-request (req &key ssl)
  (let ((puri (request-uri req))
        (http-version (http-version (request-http req)))
        (headers (request-headers req)))
    (destructuring-bind (server-name &optional server-port)
        (split-sequence #\: (getf headers :host) :from-end t)
      (nconc
       (list :request-method (request-method req)
             :script-name ""
             :server-name server-name
             :server-port (if server-port
                              (parse-integer server-port :junk-allowed t)
                              80)
             :server-protocol (intern (format nil "HTTP/~A" http-version)
                                      :keyword)
             :path-info (do-urlencode:urldecode (uri-path puri) :lenientp t)
             :query-string (uri-query puri)
             :url-scheme (if ssl :https :http)
             :request-uri (request-resource req)
             :raw-body (flex:make-in-memory-input-stream (http-parse:http-body (request-http req)))
             :content-length (getf headers :content-length)
             :content-type (getf headers :content-type)
             :clack.streaming t
             :clack.nonblocking t
             :clack.io (request-socket req))

       (loop with env-hash = (make-hash-table :test 'eq)
             for (key val) on headers by #'cddr
             unless (find key '(:request-method
                                :script-name
                                :path-info
                                :server-name
                                :server-port
                                :server-protocol
                                :request-uri
                                :remote-addr
                                :remote-port
                                :query-string
                                :content-length
                                :content-type
                                :connection))
               do
                  (let ((key (intern (format nil "HTTP-~:@(~A~)" key) :keyword)))
                    (if (gethash key env-hash)
                        (setf (gethash key env-hash)
                              (concatenate 'string (gethash key env-hash) ", " val))
                        (setf (gethash key env-hash) val)))
             finally
                (return (hash-table-plist env-hash)))))))

(defun handle-response (res clack-res)
  (let ((no-body '#:no-body))
    (flet ((handle-normal-response (res clack-res)
             (destructuring-bind (status headers &optional (body no-body)) clack-res
               (let ((stream (start-response res
                                             :status status
                                             :headers headers)))

                 ;; Returns a writer function for streaming response
                 (when (eq body no-body)
                   (return-from handle-normal-response
                     (lambda (body &key (close nil))
                       (etypecase body
                         (string (write-sequence (babel:string-to-octets body) stream))
                         (vector (write-sequence body stream)))
                       (when close
                         (finish-response res)))))

                 (prog1
                     (etypecase body
                       (null) ;; nothing to response
                       (pathname
                        (with-open-file (in body
                                            :direction :input
                                            :element-type '(unsigned-byte 8))
                          (copy-stream in stream)))
                       (list
                        (write-sequence
                         (with-fast-output (buffer :vector)
                           (let ((first t))
                             (dolist (str body)
                               (if first
                                   (setf first nil)
                                   (fast-write-byte #.(char-code #\Newline) buffer))
                               (fast-write-sequence (babel:string-to-octets str) buffer))))
                         stream))
                       ((vector (unsigned-byte 8))
                        (write-sequence body stream)))
                   (finish-response res))))))
      (etypecase clack-res
        (list (handle-normal-response res clack-res))
        (function (funcall clack-res (lambda (clack-res)
                                       (handle-normal-response res clack-res))))))))
