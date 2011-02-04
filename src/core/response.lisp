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

(clack.util:package clack.response
  (:use :cl
        :clack.util
        :clack.util.hunchentoot
        :alexandria
        :anaphora)
  (:export :<response>
           :make-response
           :finalize
           :status
           :headers
           :push-header
           :body
           :set-cookies
           :redirect))

(defclass <response> ()
     ((status :initarg :status :initform nil :accessor status)
      (headers :initarg :headers :initform nil)
      (body :initarg :body :initform nil :reader body)
      (set-cookies :initform nil))
  (:documentation "Portable HTTP Response object for Clack response."))

(defmethod initialize-instance :after ((res <response>) &rest initargs)
  (declare (ignore initargs))
  (let ((body (body res)))
    (when (stringp body)
      (setf (slot-value res 'body) (list body)))))

;; constructor
(defun make-response (&optional status headers body)
  "A synonym for (make-instance '<response> ...).
Create a <response> instance."
  (make-instance '<response>
     :status status
     :headers headers
     :body body))

(defmethod headers ((res <response>) &optional name)
  "Get whole of headers or header value of given name.

Example:
  (headers res)
  ;;=> (:content-type \"text/plain\")
  (headers res :content-type)
  ;;=> \"text/plain\"
"
  (if name
      (getf* (headers res) name)
      (slot-value res 'headers)))

(defmethod (setf headers) (value (res <response>) &optional name)
  "Set headers.

Example:
  (setf (headers res) '(:content-type \"text/html\"))
  (setf (headers res :content-type) \"text/html\")
"
  (if name
      (setf (getf* (slot-value res 'headers) name) value)
      (setf (slot-value res 'headers) value)))

(defmethod push-header ((res <response>) name value)
  "Push the given header pair into response headers.
Example: (push-header res :content-type \"text/html\")"
  (setf (headers res)
        (append (list name value) (headers res))))

(defmethod (setf body) (value (res <response>))
  "Set body with normalizing. body must be a list."
  (setf (slot-value res 'body)
        (normalize-body value)))

(defmethod set-cookies ((res <response>) &optional name)
  "Get whole of set-cookies plist or the set-cookie value of given name.

Example:
  (set-cookies res)
  ;;=> (:hoge \"1\")
  (set-cookies res :hoge)
  ;;=> \"1\"
"
  (let ((cookies (slot-value res 'set-cookies)))
    (if name
        (getf* cookies name)
        cookies)))

(defmethod (setf set-cookies) (value (res <response>) &optional name)
  "Set set-cookies.

Example:
  (setf (set-cookies res) '(:hoge \"1\"))
  (setf (set-cookies res :hoge) \"1\")
"
  (if name
      (setf (getf* (slot-value res 'set-cookies) name)
            (if (consp value)
                value
                `(:value ,value)))
      (setf (slot-value res 'set-cookies) value)))

(defmethod redirect ((res <response>) url &optional (status 302))
  "Set headers for redirecting to given url."
  (setf (headers res :location) url)
  (setf (status res) status)
  url)

(defmethod finalize ((res <response>))
  "Return a Clack response list containing three values, status, headers and body."
  (finalize-cookies res)
  (list
   (status res)
   (headers res)
   (body res)))

;;====================
;; Private methods
;;====================
(defmethod finalize-cookies ((res <response>))
  "Convert set-cookies into a header pair and push it into headers."
  (doplist (k v (set-cookies res))
    (push-header res :set-cookie (bake-cookie res k v))))

(defmethod bake-cookie ((res <response>) k v)
  "Create a string for Set-Cookie of the request header."
  (unless v (return-from bake-cookie ""))

  (let ((cookie `((,(hunchentoot:url-encode (symbol-name k))
                   ,(Hunchentoot:url-encode (getf v :value))))))
    (awhen (getf v :domain) (push `("domain" ,it) cookie))
    (awhen (getf v :path) (push `("path" ,it) cookie))
    (awhen (getf v :expires) (push `("expires" ,(format-rfc1123-timestring it)) cookie))
    (awhen (getf v :secure) (push '("secure") cookie))
    (awhen (getf v :httponly) (push '("HttpOnly") cookie))

    (format nil
            "~{~{~A~^=~}~^; ~}"
            (nreverse cookie))))

;;====================
;; Private functions
;;====================
(defun normalize-body (body)
  "body must be a list."
  (if (stringp body) (list body) body))

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
* set-cookies
* redirect

# AUTHOR

* Eitarow Fukamachi

# COPYRIGHT AND LICENSE

Copyright 2011 (c) Eitarow Fukamachi  
Licensed under the LLGPL License.

|#
