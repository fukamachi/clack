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
  (:use :cl :cl-ppcre)
  (:export :<request>
           :make-request))

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
      (clack-handler :initarg :clack-handler :initform nil :accessor clack-handler)))

(defun make-request (req)
  "Make a <request> instance from request plist."
  (apply #'make-instance '<request> req))
