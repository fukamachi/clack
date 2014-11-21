#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.response
  (:use :cl)
  (:import-from :clack.request-response
                :headers)
  (:import-from :trivial-types
                :property-list)
  (:import-from :alexandria
                :ensure-list
                :doplist
                :when-let)
  (:import-from :clack.util
                :getf*)
  (:import-from :quri
                :url-encode)
  (:import-from :local-time
                :format-timestring
                :universal-to-timestamp
                :+gmt-zone+)
  (:export :status))
(in-package :clack.response)

(cl-syntax:use-syntax :annot)

@export
(defclass <response> ()
     ((status :type (or integer null)
              :initarg :status
              :initform nil
              :accessor status)
      (headers :type property-list
               :initarg :headers
               :initform nil)
      (body :type (or list pathname function (vector (unsigned-byte 8)))
            :initarg :body
            :initform nil
            :reader body)
      (set-cookies :initform nil))
  (:documentation "Portable HTTP Response object for Clack response."))

(defmethod initialize-instance :after ((res <response>) &key)
  (when (stringp (body res))
    (setf (body res)
          `(,(body res)))))

@export
;; constructor
(defun make-response (&optional status headers body)
  "A synonym for (make-instance '<response> ...).
Create a <response> instance."
  (make-instance '<response>
     :status status
     :headers headers
     :body body))

@export
(defgeneric headers (res &optional name)
  (:documentation
   "Get all included headers, or the header value that corresponds to `name'.

Example:
  (headers res)
  ;;=> (:content-type \"text/plain\")
  (headers res :content-type)
  ;;=> \"text/plain\"")
  (:method ((res <response>) &optional name)
    (if name
        (getf* (headers res) name)
        (slot-value res 'headers))))

@export
(defgeneric (setf headers) (value res &optional name)
  (:documentation
   "Set headers.

Example:
  (setf (headers res) '(:content-type \"text/html\"))
  (setf (headers res :content-type) \"text/html\")")
  (:method (value (res <response>) &optional name)
    (if name
        (setf (getf* (slot-value res 'headers) name) value)
        (setf (slot-value res 'headers) value))))

@export
(defgeneric push-header (res name value)
  (:documentation
   "Push the given header pair into response headers.
Example: (push-header res :content-type \"text/html\")")
  (:method ((res <response>) name value)
    (setf (headers res)
          `(,name ,value ,@(headers res)))))

@export
(defgeneric (setf body) (value res)
  (:documentation
   "Set body with normalizing. body must be a list.")
  (:method (value (res <response>))
    (setf (slot-value res 'body)
          (etypecase value
            (string (ensure-list value))
            ((or list pathname function (vector (unsigned-byte 8))) value)))))

@export
(defgeneric set-cookies (res &optional name)
  (:documentation
   "Get whole of set-cookies plist or the set-cookie value of given name.

Example:
  (set-cookies res)
  ;;=> (:hoge \"1\")
  (set-cookies res :hoge)
  ;;=> \"1\"")
  (:method ((res <response>) &optional name)
    (let ((cookies (slot-value res 'set-cookies)))
      (if name
          (getf* cookies name)
          cookies))))

@export
(defgeneric (setf set-cookies) (value res &optional name)
  (:documentation
   "Set set-cookies.

Example:
  (setf (set-cookies res) '(:hoge \"1\"))
  (setf (set-cookies res :hoge) \"1\")")
  (:method (value (res <response>) &optional name)
    (if name
        (setf (getf* (slot-value res 'set-cookies) name)
              (if (consp value)
                  value
                  `(:value ,value)))
        (setf (slot-value res 'set-cookies) value))))

@export
(defgeneric redirect (res url &optional status)
  (:documentation
   "Set headers for redirecting to given url.")
  (:method ((res <response>) url &optional (status 302))
    (setf (headers res :location) url)
    (setf (status res) status)
    url))

@export
(defgeneric finalize (res)
  (:documentation
   "Return a Clack response list containing three values: status, headers, and body.")
  (:method ((res <response>))
    (finalize-cookies res)
    (list (status res)
          (headers res)
          (body res))))

(defun finalize-cookies (res)
  "Convert set-cookies into a header pair and push it into headers."
  (check-type res <response>)
  (doplist (k v (set-cookies res))
    (push-header res :set-cookie (bake-cookie res k v))))

(defun bake-cookie (res k v)
  "Create a string for Set-Cookie of the request header."
  (check-type res <response>)

  (unless v (return-from bake-cookie ""))

  (let ((cookie `((,(quri:url-encode (symbol-name k))
                   ,(quri:url-encode (getf v :value))))))
    (when-let (domain (getf v :domain))
      (push `("domain" ,domain) cookie))
    (when-let (path (getf v :path))
      (push `("path" ,path) cookie))
    (when-let (expires (getf v :expires))
      (push `("expires"
              ,(format-timestring
                nil (universal-to-timestamp expires)
                :format '(:short-weekday ", " (:day 2) #\  :short-month #\  (:year 4) #\  (:hour 2) #\:
                          (:min 2) #\: (:sec 2) " GMT")
                :timezone +gmt-zone+)) cookie))
    (when (getf v :secure)
      (push '("secure") cookie))
    (when (getf v :httponly)
      (push '("HttpOnly") cookie))

    (format nil
            "~{~{~A~^=~}~^; ~}"
            (nreverse cookie))))

(doc:start)

@doc:NAME "
Clack.Response - Portable HTTP Response object for Clack response.
"

@doc:SYNOPSIS "
    (defvar res nil)
    
    (setf res (make-response 200))
    (setf res (make-response 200 '(:content-type \"text/html\")))
    (setf res (make-response 200 '(:content-type \"text/html\") '(\"aiueo\")))
    
    ;; Access each fields
    (status res)
    ;;=> 200
    (headers res)
    ;;=> (:content-type \"text/html\")
    (headers res :content-type)
    ;;=> \"text/html\"
    (body res)
    ;;=> (\"aiueo\")
    
    ;; Set to each fields
    (setf (status res) 304)
    (setf (headers res :content-type) \"text/plain\")
    (setf (body res) '(\"moved\"))
    (setf (body res) \"moved\") ;; string also allowed
"

@doc:DESCRIPTION "
Clack.Response allows you a way to create Clack response.
"

@doc:AUTHOR "
* Eitaro Fukamachi (e.arrows@gmail.com)
"
