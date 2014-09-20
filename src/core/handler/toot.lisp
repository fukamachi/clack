#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.handler.toot
  (:use :cl
        :toot
        :split-sequence
        :ppcre)
  (:shadow :handle-request)
  (:import-from :clack.component
                :<component>
                :call)
  (:import-from :flexi-streams
                :octets-to-string)
  (:import-from :alexandria
                :if-let))
(in-package :clack.handler.toot)

(cl-syntax:use-syntax :annot)

@export
(defun run (app &key debug (port 5000))
  "Start Toot server."
  (toot:start-server
   :handler (lambda (req)
              (handle-response
               req
               (if debug
                   (call app (handle-request req))
                   (if-let (res (handler-case (call app (handle-request req))
                                  (condition () nil)))
                     res
                     '(500 nil nil)))))
   :port port))


@export
(defun stop (acceptor)
  "Stop Toot server."
  (toot:stop-acceptor acceptor))

(defun handle-request (req)
  "Convert Request from server into a plist
before pass to Clack application."
  (let ((content-length (if-let (content-length (request-header :content-length req))
                          (parse-integer content-length :junk-allowed t)
                          (setf (slot-value req 'request-headers) (acons :content-length "" (slot-value req 'request-headers))))))
    (destructuring-bind (server-name &optional server-port)
        (split-sequence #\: (cdr (assoc :host (request-headers req))))
      (append
       (list
        :request-method (request-method req)
        :script-name ""
        :path-info (url-decode (request-path req))
        :server-name server-name
        :server-port (if server-port
                         (parse-integer server-port)
                         80)
        :server-protocol (server-protocol req)
        :request-uri (request-uri req)
        :url-scheme :HTTP    ;(request-scheme req)
        :remote-addr (remote-addr req)
        :remote-port (remote-port req)
        :query-string (request-query req)
        :content-length content-length
        :content-type (request-header :content-type req)
        :raw-body (toot::request-body-stream req)
        :clack.streaming t
        :clack.handler :toot)

       (loop for (k . v) in (toot::request-headers req)
             unless (find k '(:request-method :script-name :path-info :server-name :server-port :server-protocol :request-uri :remote-addr :remote-port :query-string :content-length :content-type :accept :connection))
               append (list (intern (format nil "HTTP-~:@(~A~)" k) :keyword)
                            v))))))

(defun handle-response (req res)
  (let ((no-body '#:no-body))
    (flet ((handle-normal-response (req res)
             (destructuring-bind (status headers &optional (body no-body)) res
               (when (pathnamep body)
                 (multiple-value-call #'serve-file
                   (values req body (parse-charset (getf headers :content-type))))
                 (return-from handle-normal-response))

               (setf (status-code req) status)
               (loop for (k v) on headers by #'cddr
                     with hash = (make-hash-table :test #'eq)
                     if (gethash k hash)
                       do (setf (gethash k hash)
                                (format nil "~:[~;~:*~A, ~]~A" (gethash k hash) v))
                     else if (eq k :content-type)
                            do (multiple-value-bind (v charset)
                                   (parse-charset v)
                                 (setf (gethash k hash) v)
                                 (setf (toot::response-charset req) charset))
                     else do (setf (gethash k hash) v)
                     finally
                        (loop for k being the hash-keys in hash
                                using (hash-value v)
                              do (setf (response-header k req) v)))
               (toot::send-response-headers
                req
                (getf headers :content-length)
                nil
                (toot::response-charset req))
               (let ((out (toot::content-stream req)))
                 (when (eq body no-body)
                   (return-from handle-normal-response
                     (lambda (body &key (close nil))
                       (declare (ignore close))
                       (write-sequence (if (stringp body)
                                           (flex:string-to-octets body :external-format toot::*default-charset*)
                                           body)
                                       out))))

                 (etypecase body
                   (null) ;; nothing to response
                   (list
                    (write-sequence (flex:string-to-octets (format nil "~{~A~^~%~}" body)
                                                           :external-format toot::*default-charset*)
                                    out)))))))
      (etypecase res
        (list (handle-normal-response req res))
        (function (funcall res (lambda (res)
                                 (handle-normal-response req res))))))))

(defun parse-charset (content-type)
  (multiple-value-bind (start end reg1 reg2)
      (ppcre:scan "(;\\s*?charset=([-_a-zA-Z0-9]+))" content-type)
    (declare (ignore end))
    (if start
        (values (subseq content-type 0 (aref reg1 0))
                (subseq content-type (aref reg1 1) (aref reg2 1)))
        ;; there is no ";charset="
        (values content-type toot::*default-charset*))))
