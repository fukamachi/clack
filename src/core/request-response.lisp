(in-package :cl-user)
(defpackage clack.request-response
  (:use :cl)
  (:export :headers))
(in-package :clack.request-response)

(defgeneric headers (object &optional name))

(defgeneric (setf headers) (headers object &optional name))
