#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.request
  (:use :cl
        :anaphora
        :metabang-bind)
  (:import-from :flexi-streams
                :make-external-format
                :make-flexi-stream)
  (:import-from :cl-ppcre
                :split
                :scan-to-strings
                :register-groups-bind)
  (:import-from :clack.util
                :getf-all
                :merge-plist)
  (:export :request-method
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

           :referer
           :user-agent
           :uploads))

(cl-annot:enable-annot-syntax)

@export
(defclass <request> ()
     ((request-method :initarg :request-method :initform nil
                      :reader request-method
                      :documentation "The HTTP request method.
This must be one of :GET, :HEAD, :OPTIONS, :PUT, :POST, or :DELETE.")
      (script-name :initarg :script-name :initform nil :reader script-name
                   :documentation "The initial portion of the request URL's path, corresponding to the application.
This may be an empty string if the application corresponds to the server's root URI. If this key is not empty, it must start with a forward slash (/).")
      (path-info :initarg :path-info :initform nil :reader path-info
                 :documentation "The remainder of the request URL's path.
This may be an empty string if the request URL targets the application root and does no have a trailing slash.")
      (server-name :initarg :server-name :initform nil :reader server-name
                   :documentation "The resolved server name, or the server IP address.")
      (server-port :initarg :server-port :initform nil :reader server-port
                   :documentation "The port on which the request is being handled.")
      (server-protocol :initarg :server-protocol :initform nil
                       :reader server-protocol
                       :documentation "The version of the protocol the client used to send the request.
Typically this will be something like :HTTP/1.0 or :HTTP/1.1.")
      (request-uri :initarg :request-uri :initform nil :reader request-uri
                   :documentation "The request URI. Must start with '/'.")
      (uri-scheme :initarg :uri-scheme :initform nil :reader uri-scheme)
      (remote-addr :initarg :remote-addr :initform nil :reader remote-addr)
      (remote-port :initarg :remote-port :initform nil :reader remote-port)
      (query-string :initarg :query-string :initform nil :reader query-string
                    :documentation "The portion of the request URL that follows the '?', if any.")
      (raw-body :initarg :raw-body :initform nil :reader raw-body)
      (content-length :initarg :content-length :initform nil
                      :reader content-length)
      (content-type :initarg :content-type :initform nil :reader content-type)
      (clack-handler :initarg :clack-handler :initform nil :reader clack-handler)

      (http-referer :initarg :http-referer :initform nil :reader referer)
      (http-user-agent :initarg :http-user-agent :initform nil :reader user-agent)
      (http-cookie :initarg :http-cookie :initform nil)

      (body-parameters :initform nil)
      (query-parameters :initform nil)
      (uploads :initarg :clack.uploads :initform nil :accessor uploads))
  (:documentation "Portable HTTP Request object for Clack Request."))

(defmethod initialize-instance :after ((this <request>) &key)
  ;; cookies
  (swhen (slot-value this 'http-cookie)
    (setf it (parameters->plist it)))

  ;; GET parameters
  (setf (slot-value this 'query-parameters)
        (parameters->plist (query-string this)))

  ;; POST parameters
  (bind ((body (raw-body this))
         ((:values type subtype charset)
          (parse-content-type (content-type this)))
         (content-type (concatenate 'string type "/" subtype))
         (external-format
          (flex:make-external-format
           (if charset
               (intern (string-upcase charset) :keyword)
               :utf-8)
           :eol-style :lf)))
    (cond
      ((string= content-type "application/x-www-form-urlencoded")
       (setf (slot-value this 'body-parameters)
             (parameters->plist (read-line body nil ""))))
      ((and (string= content-type "multipart/form-data")
            (not (uploads this))) ;; not set yet.
       (setf (uploads this)
             (clack.util.hunchentoot:parse-rfc2388-form-data
              (flex:make-flexi-stream body)
              content-type
              external-format))))))

@export
;; constructor
(defun make-request (req)
  "A synonym for (make-instance '<request> ...).
Make a <request> instance from request plist."
  (apply #'make-instance '<request> :allow-other-keys t req))

@export
(defmethod securep ((req <request>))
  (eq (uri-scheme req) :https))

@export
(defmethod cookies ((req <request>) &optional name)
  "Returns cookies as a plist. If optional `name' is specified, returns the value corresponds to it."
  (let ((params (slot-value req 'http-cookie)))
    (if name
        (getf-all params name)
        params)))

@export
(defmethod body-parameter ((req <request>) &optional name)
  "Return POST parameters as a plist. If optional `name' is specified, returns the value corresponds to it."
  (let ((params (slot-value req 'body-parameters)))
    (if name
        (getf-all params name)
        params)))

@export
(defmethod query-parameter ((req <request>) &optional name)
  "Returns GET parameters as a plist. If optional `name' is specified, returns the value corresponds to it."
  (let ((params (slot-value req 'query-parameters)))
    (if name
        (getf-all params name)
        params)))

@export
(defmethod parameter ((req <request>) &optional name)
  "Returns request parameters containing (merged) GET and POST parameters. If optional `name' is specified, returns the value corresponds to it."
  (let ((params (merge-plist (query-parameter req)
                             (body-parameter req))))
    (if name
        (getf-all params name)
        params)))

(defun parameters->plist (params)
  "Convert parameters into plist. The `params' must be a string."
  (loop for kv in (ppcre:split "&" params)
        for (k v) = (ppcre:split "=" kv)
        append (list (intern k :keyword)
                     ;; KLUDGE: calls `ignore-errors'.
                     (or (ignore-errors (clack.util.hunchentoot:url-decode v)) v))))

(defun parse-content-type (content-type)
  "Parse Content-Type from Request header."
  (ppcre:register-groups-bind (type subtype params)
      ("^(.+?)/([^;]+);?(?:(.+)|$)" content-type)
    (values
     (or type "application")
     (or subtype "octet-stream")
     (awhen params
       (aand (nth-value 1 (ppcre:scan-to-strings
                           "charset=([^; ]+)" params))
             (aref it 0))))))

(doc:start)

@doc:NAME "
Clack.Request - Portable HTTP Request object for Clack Request.
"

@doc:SYNOPSIS "
    (defun app (req)
      (let ((req (make-request req)))
      `(200
        (:content-type \"text/plain\")
        (\"Hello, \" (query-parameter req \"name)))))
"

@doc:DESCRIPTION "
Clack.Request provides a consistent API for request objects.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
