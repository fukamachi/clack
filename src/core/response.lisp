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
