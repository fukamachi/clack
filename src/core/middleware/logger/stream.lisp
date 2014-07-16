#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.logger.stream
  (:use :cl)
  (:import-from :clack.logger.base
                :<clack-logger-base>
                :output))
(in-package :clack.logger.stream)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-logger-stream> (<clack-logger-base>)
     ((output-stream :type stream
                     :initarg :output-stream
                     :initform *standard-output*
                     :accessor output-stream)))

(defmethod output ((this <clack-logger-stream>) message)
  "Output log messages to 'output-stream' in this slot."
  (write-string message (output-stream this)))

(doc:start)

@doc:NAME "
Clack.Logger.Stream - Output log messages to stream.
"

@doc:SYNOPSIS "
    (clackup (builder
              (<clack-middleware-logger>
               :logger (make-instance '<clack-logger-stream>))
              (lambda (env)
                (log-message :notice \"You've got an access!\")
                '(200 nil (\"ok\")))))
"

@doc:DESCRIPTION "
Clack.Logger.Stream is a logger the outputs log messages to a stream, using `*standard-output*' as the default.

This logger is used in Clack.Middleware.Logger as the default logger.
"

@doc:AUTHOR "
* Eitaro Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Logger.Base
* Clack.Middleware.Logger
* Clack.Logger
"
