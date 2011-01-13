#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack environment to pass to Application or Middlewares.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :clack.core)

(defclass <environment> ()
     ((request-method :initarg :request-method :accessor request-method)
      (script-name :initarg :script-name :accessor script-name)
      (path-info :initarg :path-info :accessor path-info)
      (request-uri :initarg :request-uri :accessor request-uri)
      (query-string :initarg :query-string :accessor query-string)
      (server-name :initarg :server-name :accessor server-name)
      (server-port :initarg :server-port :accessor server-port)
      (server-protocol :initarg :server-protocol :accessor server-protocol)
      (%request :initarg :request))
  (:documentation "Clack environment class to pass to Application or Middlewares."))
