#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.logger.stream
  (:use :cl
        :anaphora)
  (:import-from :clack.logger :*logger-output*)
  (:import-from :clack.logger.base
                :<clack-logger-base>
                :output))

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-logger-stream> (<clack-logger-base>)
     ((output-stream :type stream
                     :initarg :output-stream
                     :initform *standard-output*
                     :accessor output-stream)))

(defmethod output ((this <clack-logger-stream>))
  "Output log messages to 'output-stream' in this slot."
  (awhen (get-output-stream-string *logger-output*)
    (write-string it (output-stream this))))

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
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Logger.Base
* Clack.Middleware.Logger
* Clack.Logger
"
