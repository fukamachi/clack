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
     ((request-method :initarg :request-method :initform nil :accessor request-method)
      (script-name :initarg :script-name :initform nil :accessor script-name)
      (path-info :initarg :path-info :initform nil :accessor path-info)
      (server-name :initarg :server-name :initform nil :accessor server-name)
      (server-port :initarg :server-port :initform nil :accessor server-port)
      (server-protocol :initarg :server-protocol :initform nil :accessor server-protocol)
      (request-uri :initarg :request-uri :initform nil :accessor request-uri)
      (uri-scheme :initarg :uri-scheme :initform nil :accessor uri-scheme)
      (remote-addr :initarg :remote-addr :initform nil :accessor remote-addr)
      (remote-port :initarg :remote-port :initform nil :accessor remote-port)
      (query-string :initarg :query-string :initform nil :accessor query-string)
      (raw-body :initarg :raw-body :initform nil :accessor raw-body)
      (content-length :initarg :content-length :initform nil :accessor content-length)
      (content-type :initarg :content-type :initform nil :accessor content-type)
      (clack-handler :initarg :clack-handler :initform nil :accessor clack-handler)

      (http-referer :initarg :http-referer :initform nil)
      (http-user-agent :initarg :http-user-agent :initform nil)
      (http-cookie :initarg :http-cookie :initform nil)
      (cookies :initform nil)

      (body-parameters :initform nil)
      (query-parameters :initform nil)))

(defun make-request (req)
  "Make a <request> instance from request plist."
  (apply #'make-instance '<request> :allow-other-keys t req))

(defmethod referer ((req <request>))
  "Returns referer uri."
  (slot-value req 'http-referer))

(defmethod user-agent ((req <request>))
  "Returns user agent of the client."
  (slot-value req 'http-user-agent))

(defmethod cookies ((req <request>) &optional name)
  "Returns cookies as a plist. If optional `name' is specified, returns the value corresponds to it."
  (sunless (slot-value req 'cookies)
    (setf it
          (parameters->plist (slot-value req 'http-cookie))))

  (let ((params (slot-value req 'cookies)))
    (if name
        (getf-all params name)
        params)))

(defmethod body-parameters ((req <request>) &optional name)
  "Return POST parameters as a plist. If optional `name' is specified, returns the value corresponds to it."
  (sunless (slot-value req 'body-parameters)
    (setf it
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
               (parameters->plist (read-line body nil "")))
              ((string= content-type "multipart/form-data")
               ;; FIXME: depends on Hunchentoot.
               (hunchentoot::parse-rfc2388-form-data
                (flex:make-flexi-stream body)
                content-type
                external-format))))))

  (let ((params (slot-value req 'body-parameters)))
    (if name
        (getf-all params name)
        params)))

(defmethod query-parameters ((req <request>) &optional name)
  "Returns GET parameters as a plist. If optional `name' is specified, returns the value corresponds to it."
  (sunless (slot-value req 'query-parameters)
    (setf it (parameters->plist (query-string req))))

  (let ((params (slot-value req 'query-parameters)))
    (if name
        (getf-all params name)
        params)))

(defmethod parameters ((req <request>) &optional name)
  "Returns request parameters containing (merged) GET and POST parameters. If optional `name' is specified, returns the value corresponds to it."
  (let ((params (merge-plist (query-parameter req)
                             (body-parameter req))))
    (if name
        (getf-all params name)
        params)))

(defun merge-plist (p1 p2)
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

(defmethod securep ((req <request>))
  (eq (uri-scheme req) :https))

#|
=markdown

# NAME

clack.request - Provide easy accessing to Clack Request.

# SYNOPSIS

    (defun app (req)
      (let ((req (make-request req)))
      `(200
        (:content-type "text/plain")
        ("Hello, " (query-parameters req "name)))))

# DESCRIPTION

clack.request provides a consistent API for request objects.

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
