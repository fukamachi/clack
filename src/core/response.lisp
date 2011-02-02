#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Portable HTTP Response object for Clack response.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)

(defpackage clack.response
  (:use :cl
        :alexandria)
  (:export :<response>
           :make-response
           :finalize
           :status
           :headers
           :body
           :cookies
           :redirect))

(in-package :clack.response)

(defclass <response> ()
     ((status :initarg :status :initform nil :accessor status)
      (headers :initarg :headers :initform nil)
      (body :initarg :body :initform nil :reader body)
      (cookies :initform nil)))

(defun make-response (&optional status headers body)
  "Create <response> instance."
  (make-instance '<response>
     :status status
     :headers headers
     :body body))

(defmethod headers ((res <response>) &optional name)
  "Get header value of given name."
  (if name
      (getf (headers res) (normalize-key name))
      (slot-value res 'headers)))

(defmethod (setf headers) (value (res <response>) name)
  (setf (getf (slot-value res 'headers) (normalize-key name)) value))

(defmethod (setf body) (value (res <response>))
  (setf (slot-value res 'body)
        (if (stringp value) (list value) value)))

(defmethod cookies ((res <response>) &optional name)
  (let ((cookies (slot-value res 'cookies)))
    (if name
        (getf cookies (normalize-key name))
        cookies)))

(defmethod (setf cookies) (value (res <response>) name)
  (setf (getf (slot-value res 'cookies) (normalize-key name)) value))

(defmethod redirect ((res <response>) url &optional (status 302))
  "Set headers for redirecting to given url."
  (setf (headers res :location) url)
  (setf (status res) status)
  url)

(defmethod finalize ((res <response>))
  "Return Clack response list."
  (finalize-cookies res)
  (list
   (status res)
   (headers res)
   (body res)))

(defmethod finalize-cookies ((res <response>))
  (doplist (k v (cookies res))
    (setf (headers res :set-cookie) (bake-cookie res k v))))

(defmethod bake-cookie ((res <response>) k v)
  (unless v (return-from bake-cookie ""))

  ;; TODO: domain, path, expires, secure, HttpOnly
  (format nil
          "~{~A=~A~^; ~}"
          (list (hunchentoot:url-encode (symbol-name k))
                (hunchentoot:url-encode v))))

(defun normalize-key (name)
  (etypecase name
    (string (intern name :keyword))
    (keyword name)
    (symbol (intern (symbol-name name) :keyword))))

#|
=markdown

# NAME

clack.response - Provide easy accessing to Clack Request.

# SYNOPSIS

    (defvar res nil)
    
    (setf res (make-response 200))
    (setf res (make-response 200 '(:content-type "text/html")))
    (setf res (make-response 200 '(:content-type "text/html") '("aiueo")))
    
    ;; Access each fields
    (status res)
    ;;=> 200
    (headers res)
    ;;=> (:content-type "text/html")
    (headers res :content-type)
    ;;=> "text/html"
    (body res)
    ;;=> ("aiueo")
    
    ;; Set to each fields
    (setf (status res) 304)
    (setf (headers res :content-type) "text/plain")
    (setf (body res) '("moved"))
    (setf (body res) "moved") ;; string also allowed

# DESCRIPTION

clack.response allows you a way to create Clack response.

## Functions

* make-response
* status
* headers
* body
* finalize
* cookies
* redirect

# AUTHOR

* Eitarow Fukamachi

# COPYRIGHT AND LICENSE

Copyright 2011 (c) Eitarow Fukamachi  
Licensed under the LLGPL License.

|#
