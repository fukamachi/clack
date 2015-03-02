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
                :send-response
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
  (:import-from :http-parse
                :http-body
                :http-store-body
                :http-version)
  (:import-from :quri
                :uri-path
                :uri-query)
  (:import-from :quri
                :parse-uri
                :url-decode)
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
  (let ((*state* (make-instance 'wookie:wookie-state)))
    (add-hook :parsed-headers 'parsed-headers-hook :clack-handler-wookie-parsed-headers-hook)
    (defroute (:* ".*" :chunk nil) (req res)
      (let ((env (handle-request req :ssl ssl)))
        (handle-response
         res
         (if debug
             (call app env)
             (handler-case (call app env)
               (error (error)
                 (princ error *error-output*)
                 '(500 nil ("Internal Server Error"))))))))
    (handler-case
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
            (start-server listener)))
      (as:socket-closed () nil))))

@export
(defun stop (server)
  (as:close-tcp-server server))

(defun handle-request (req &key ssl)
  (let ((quri (request-uri req))
        (http-version (http-version (request-http req)))
        (headers (make-hash-table :test 'equal))
        content-length
        content-type)
    (loop for (key val) on (request-headers req) by #'cddr
          if (eq key :content-length)
            do (setf content-length val)
          else if (eq key :content-type)
            do (setf content-type val)
          else
            do (let ((key (string-downcase key)))
                 (multiple-value-bind (current existsp)
                     (gethash key headers)
                   (setf (gethash key headers)
                         (if existsp
                             (format nil "~A, ~A" current val)
                             val)))))

    (destructuring-bind (server-name &optional server-port)
        (split-sequence #\: (gethash "host" headers "") :from-end t :count 2)
      (setf (quri:uri-path quri)
            (nth-value 4 (quri:parse-uri (request-resource req))))
      (list :request-method (request-method req)
            :script-name ""
            :server-name server-name
            :server-port (if server-port
                             (parse-integer server-port :junk-allowed t)
                             80)
            :server-protocol (intern (format nil "HTTP/~A" http-version)
                                     :keyword)
            :path-info (handler-case (quri:url-decode (uri-path quri))
                         (quri:uri-malformed-string ()
                           (uri-path quri)))
            :query-string (uri-query quri)
            :url-scheme (if ssl :https :http)
            :request-uri (request-resource req)
            :raw-body (flex:make-flexi-stream
                       (flex:make-in-memory-input-stream (http-parse:http-body (request-http req)))
                       :external-format :utf-8)
            :content-length content-length
            :content-type content-type
            :clack.streaming t
            :clack.nonblocking t
            :clack.io (request-socket req)
            :headers headers))))

(defun handle-response (res clack-res)
  (etypecase clack-res
    (list (handle-normal-response res clack-res))
    (function (funcall clack-res (lambda (clack-res)
                                   (handler-case
                                       (handle-normal-response res clack-res)
                                     ;; Ignore when the socket is closed.
                                     (as:socket-closed ())))))))

(defun handle-normal-response (res clack-res)
  (let ((no-body '#:no-body))
    (destructuring-bind (status headers &optional (body no-body)) clack-res
      ;; Returns a writer function for streaming response
      (when (eq body no-body)
        (let ((stream (start-response res
                                      :status status
                                      :headers headers)))
          (return-from handle-normal-response
            (lambda (body &key (close nil))
              (etypecase body
                (string (write-sequence (babel:string-to-octets body) stream))
                (vector (write-sequence body stream)))
              (when close
                (finish-response res))))))

      (etypecase body
        (null) ;; nothing to response
        (pathname
         (let ((stream (start-response res
                                       :status status
                                       :headers headers)))
           (with-open-file (in body
                               :direction :input
                               :element-type '(unsigned-byte 8))
             (copy-stream in stream))
           (finish-response res)))
        (list
         (send-response res
                        :status status
                        :headers headers
                        :body (if (null (cdr body))
                                  (car body)
                                  (with-fast-output (buffer :vector)
                                    (dolist (str body)
                                      (fast-write-sequence (babel:string-to-octets str) buffer))))
                        :close t))
        ((vector (unsigned-byte 8))
         (send-response res
                        :status status
                        :headers headers
                        :body body
                        :close t))))))
