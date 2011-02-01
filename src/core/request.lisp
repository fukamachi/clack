#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Provide easy accessing to Clack Request.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)

(defpackage clack.request
  (:use :cl
        :cl-ppcre
        :flexi-streams
        :metabang-bind
        :anaphora)
  (:export :<request>
           :make-request
           :request-method
           :script-name
           :path-info
           :server-name
           :server-port
           :server-protocol
           :request-uri
           :uri-scheme
           :remote-addr
           :remote-port
           :query-string
           :raw-body
           :content-length
           :content-type
           :clack-handler
           :body-parameters
           :query-parameters
           ))

(in-package :clack.request)

(defclass <request> ()
     ((request-method :initarg :request-method :initform nil :accessor request-method)
      (script-name :initarg :script-name :initform nil :accessor script-name)
      (path-info :initarg :path-info :initform nil :accessor path-info)
      (server-name :initarg :server-name :initform nil :accessor server-name)
      (server-port :initarg :server-port :initform nil :accessor server-port)
      (server-protocol :initarg :server-protocol :initform nil :accessor server-protocol)
      (request-uri :initarg :request-uri :initform nil :accessor request-uri)
      (uri-scheme :initarg :url-scheme :initform nil :accessor uri-scheme)
      (remote-addr :initarg :remote-addr :initform nil :accessor remote-addr)
      (remote-port :initarg :remote-port :initform nil :accessor remote-port)
      (query-string :initarg :query-string :initform nil :accessor query-string)
      (raw-body :initarg :raw-body :initform nil :accessor raw-body)
      (content-length :initarg :content-length :initform nil :accessor content-length)
      (content-type :initarg :content-type :initform nil :accessor content-type)
      (clack-handler :initarg :clack-handler :initform nil :accessor clack-handler)

      (body-parameters :initform nil)
      (query-parameters :initform nil)))

(defun make-request (req)
  "Make a <request> instance from request plist."
  (apply #'make-instance '<request> :allow-other-keys t req))

;(defmethod initialize-instance :after ((req <request>) &rest initargs))

(defmethod body-parameters ((req <request>))
  (awhen (slot-value req 'body-parameters)
    (return-from body-parameters it))

  (setf (slot-value req 'body-parameters)
        (bind ((body (raw-body req))
               ((:values type subtype charset)
                (parse-content-type (content-type req)))
               (content-type (concatenate 'string type "/" subtype))
               (external-format
                (flex:make-external-format
                 (if charset
                     (intern (string-upcase charset) :keyword)
                     :utf-8)
                 :eol-style :lf)))
          (cond
            ((string= content-type "application/x-www-form-urlencoded")
             (parameters->plist (read-line body)))
            ((string= content-type "multipart/form-data")
             ;; FIXME: depends on Hunchentoot.
             (hunchentoot::parse-rfc2388-form-data
              (flex:make-flexi-stream body)
              content-type
              external-format)))))

  (slot-value req 'body-parameters))

(defmethod query-parameters ((req <request>))
  (awhen (slot-value req 'query-parameters)
    (return-from query-parameters it))

  (setf (slot-value req 'query-parameters)
        (parameters->plist (query-string req)))

  (slot-value req 'query-parameters))

(defun parameters->plist (params)
  (loop for kv in (ppcre:split "&" params)
        for (k v) = (ppcre:split "=" kv)
        append (list (intern k :keyword)
                     ;; FIXME: depends on Hunchentoot.
                     ;;   and calls `ignore-errors'.
                     (or (ignore-errors (hunchentoot:url-decode v)) v))))

(defun parse-content-type (content-type)
  (register-groups-bind (type subtype params)
      ("^(.+?)/([^;]+);?(?:(.+)|$)" content-type)
    (values
     (or type "application")
     (or subtype "octet-stream")
     (awhen params
       (aand (nth-value 1 (ppcre:scan-to-strings
                           "charset=([^; ]+)" params))
             (aref it 0))))))
