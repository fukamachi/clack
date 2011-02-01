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
  (:use :cl)
  (:export :<response>
           :make-response
           :finalize
           :status
           :headers
           :body
           :header))

(in-package :clack.response)

(defclass <response> ()
     ((status :initarg :status :initform nil :accessor status)
      (headers :initarg :headers :initform nil :accessor headers)
      (body :initarg :body :initform nil :reader body)))

(defun make-response (&optional status headers body)
  "Create <response> instance."
  (make-instance '<response>
     :status status
     :headers headers
     :body body))

(defmethod header ((res <response>) name)
  "Get header value of given name."
  (getf (headers res) name))

(defmethod (setf header) (value (res <response>) name)
  (setf (getf (headers res) name) value))

(defmethod (setf body) (value (res <response>))
  (setf (slot-value res 'body)
        (if (stringp value) (list value) value)))

(defmethod finalize ((res <response>))
  "Return Clack response list."
  (list
   (status res)
   (headers res)
   (body res)))

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
    (header res :content-type)
    ;;=> "text/html"
    (body res)
    ;;=> ("aiueo")
    
    ;; Set to each fields
    (setf (status res) 304)
    (setf (header res :content-type) "text/plain")
    (setf (body res) '("moved"))
    (setf (body res) "moved") ;; string also allowed

# DESCRIPTION

clack.response allows you a way to create Clack response.

## Functions

* make-response
* status
* headers
* header
* body
* finalize

# AUTHOR

* Eitarow Fukamachi

# COPYRIGHT AND LICENSE

Copyright 2011 (c) Eitarow Fukamachi  
Licensed under the LLGPL License.

|#
