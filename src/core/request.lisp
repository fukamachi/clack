#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Portable HTTP Request object for Clack Request.

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

           :securep
           :referer
           :user-agent
           :cookies
           :body-parameters
           :query-parameters
           :parameters
           ))

(in-package :clack.request)

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
      (query-parameters :initform nil))
  (:documentation "Portable HTTP Request object for Clack Request."))

(defmethod initialize-instance :after ((this <request>) &rest initargs)
  (declare (ignore initargs))

  ;; cookies
  (swhen (slot-value this 'http-cookie)
    (setf it (parameters->plist it)))

  ;; GET parameters
  (setf (slot-value this 'query-parameters)
        (parameters->plist (query-string this)))

  ;; POST parameters
  (setf (slot-value this 'body-parameters)
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
             (parameters->plist (read-line body nil "")))
            ((string= content-type "multipart/form-data")
             ;; FIXME: depends on Hunchentoot.
             (hunchentoot::parse-rfc2388-form-data
              (flex:make-flexi-stream body)
              content-type
              external-format))))))

;; constructor
(defun make-request (req)
  "A synonym for (make-instance '<request> ...).
Make a <request> instance from request plist."
  (apply #'make-instance '<request> :allow-other-keys t req))

(defmethod securep ((req <request>))
  (eq (uri-scheme req) :https))

(defmethod cookies ((req <request>) &optional name)
  "Returns cookies as a plist. If optional `name' is specified, returns the value corresponds to it."
  (let ((params (slot-value req 'http-cookie)))
    (if name
        (getf-all params name)
        params)))

(defmethod body-parameters ((req <request>) &optional name)
  "Return POST parameters as a plist. If optional `name' is specified, returns the value corresponds to it."
  (let ((params (slot-value req 'body-parameters)))
    (if name
        (getf-all params name)
        params)))

(defmethod query-parameters ((req <request>) &optional name)
  "Returns GET parameters as a plist. If optional `name' is specified, returns the value corresponds to it."
  (let ((params (slot-value req 'query-parameters)))
    (if name
        (getf-all params name)
        params)))

(defmethod parameters ((req <request>) &optional name)
  "Returns request parameters containing (merged) GET and POST parameters. If optional `name' is specified, returns the value corresponds to it."
  (let ((params (merge-plist (query-parameters req)
                             (body-parameters req))))
    (if name
        (getf-all params name)
        params)))

;;====================
;; Private functions
;;====================
(defun parameters->plist (params)
  "Convert parameters into plist. The `params' must be a string."
  (loop for kv in (ppcre:split "&" params)
        for (k v) = (ppcre:split "=" kv)
        append (list (intern k :keyword)
                     ;; FIXME: depends on Hunchentoot.
                     ;;   and calls `ignore-errors'.
                     (or (ignore-errors (hunchentoot:url-decode v)) v))))

(defun parse-content-type (content-type)
  "Parse Content-Type from Request header."
  (register-groups-bind (type subtype params)
      ("^(.+?)/([^;]+);?(?:(.+)|$)" content-type)
    (values
     (or type "application")
     (or subtype "octet-stream")
     (awhen params
       (aand (nth-value 1 (ppcre:scan-to-strings
                           "charset=([^; ]+)" params))
             (aref it 0))))))

;;====================
;; Utilities
;;====================
(defun merge-plist (p1 p2)
  "Merge two plist into one plist."
  (loop with notfound = '#:notfound
        for (indicator value) on p1 by #'cddr
        when (eq (getf p2 indicator notfound) notfound) 
          do (progn
               (push value p2)
               (push indicator p2)))
  p2)

(defun getf-all (plist key)
  "This is a version of `getf' enabled to manage multiple keys. If the `plist' has two or more pairs that they have given `key' as a key, returns the values of each pairs as one list."
  (loop with params = nil
        for (k v) on plist by #'cddr
        if (string= k key)
          do (push v params)
        finally (return (if (cdr params)
                            (nreverse params)
                            (car params)))))

#|
=markdown

# NAME

clack.request - Portable HTTP Request object for Clack Request.

# SYNOPSIS

    (defun app (req)
      (let ((req (make-request req)))
      `(200
        (:content-type "text/plain")
        ("Hello, " (query-parameters req "name)))))

# DESCRIPTION

clack.request provides a consistent API for request objects.

## <request>

## Functions

* make-request
* query-parameters
* body-parameters
* parameters
* cookies

# AUTHOR

* Eitarow Fukamachi

# COPYRIGHT AND LICENSE

Copyright 2011 (c) Eitarow Fukamachi  
Licensed under the LLGPL License.

|#
