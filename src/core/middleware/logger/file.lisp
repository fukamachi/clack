#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.logger.file
  (:use :cl)
  (:import-from :clack.logger.base
                :<clack-logger-base>
                :output))
(in-package :clack.logger.file)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-logger-file> (<clack-logger-base>)
     ((output-file :type (or string pathname)
                   :initarg :output-file
                   :initform (error ":output-file is required.")
                   :accessor output-file
                   :documentation "Where to output log messages.")))

(defmethod output ((this <clack-logger-file>) message)
  "Output log messages to 'output-file' in this slot."
  (with-open-file (stream (output-file this)
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (write-string message stream)))

(doc:start)

@doc:NAME "
Clack.Logger.File - Output log messages to a file.
"

@doc:SYNOPSIS "
    (clackup (builder
              (<clack-middleware-logger>
               :logger (make-instance '<clack-logger-file>
                          :output-file #p\"~/server.log\"))
              (lambda (env)
                (log-message :notice \"You've got an access!\")
                '(200 nil (\"ok\")))))
"

@doc:AUTHOR "
* Eitaro Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Logger.Base
* Clack.Middleware.Logger
* Clack.Logger
"
