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
                :destroy-thread)
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
                :make-in-memory-input-stream
                :string-to-octets)
  (:import-from :split-sequence
                :split-sequence)
  (:import-from :alexandria
                :if-let
                :hash-table-plist))
(in-package :clack.handler.wookie)

(cl-syntax:use-syntax :annot)

;; XXX: :store-body keeps the whole POST data in-memory.
(defun parsed-headers-hook (request)
  (setf (http-parse:http-store-body (request-http request)) t))

@export
(defun run (app &key debug (port 5000))
  (prog1
      (#+thread-support bt:make-thread
       #-thread-support funcall
       #'(lambda ()
           (let ((*state* (make-instance 'wookie:wookie-state)))
             (add-hook :parsed-headers 'parsed-headers-hook :clack-handler-wookie-parsed-headers-hook)
             (defroute (:* ".*" :chunk nil) (req res)
               (let ((env (handle-request req)))
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
                   (start-server (make-instance 'wookie:listener
                                                :port port)))
               (log:config :info)))))
    ;; XXX: Wait until the TCP server is ready.
    (sleep 1)))

@export
(defun stop (server)
  (#+thread-support bt:destroy-thread
   #-thread-support as:close-tcp-server
   server))

(defun handle-request (req)
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
             :url-scheme :http
             :request-uri (request-resource req)
             :raw-body (flex:make-in-memory-input-stream (http-parse:http-body (request-http req)))
             :content-length (getf headers :content-length)
             :content-type (getf headers :content-type)
             :clack.streaming t
             :clack.nonblocking t)

       (loop with env-hash = (make-hash-table :test #'eq)
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
  (flet ((handle-normal-response (res clack-res)
           (destructuring-bind (status headers &optional body) clack-res
             (let ((stream (start-response res
                                           :status status
                                           :headers headers)))
               (prog1
                   (etypecase body
                     (null
                      (lambda (body &key (close nil))
                        (declare (ignore close))
                        (if (and (not (stringp body))
                                 (vectorp body))
                            (write-sequence body stream)
                            (write-sequence (flex:string-to-octets body) stream))
                        (if close
                            (finish-response res)
                            (force-output stream))))
                     (pathname
                      (with-open-file (in body
                                          :direction :input
                                          :element-type '(unsigned-byte 8))
                        (let ((buf (make-array (file-length in) :element-type '(unsigned-byte 8))))
                          (read-sequence buf in)
                          (write-sequence buf stream))))
                     (list
                      (write-sequence
                       (flex:string-to-octets (format nil "~{~A~^~%~}" body))
                       stream))
                     ((vector (unsigned-byte 8))
                      (write-sequence body stream)))

                 (finish-response res))))))
    (etypecase clack-res
      (list (handle-normal-response res clack-res))
      (function (funcall clack-res (lambda (clack-res)
                                     (handle-normal-response res clack-res)))))))
