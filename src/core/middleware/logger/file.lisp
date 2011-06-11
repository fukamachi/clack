#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.logger.file
  (:use :cl)
  (:import-from :clack.logger.stream
                :<clack-logger-stream>
                :output-stream)
  (:import-from :clack.logger.base
                :output))

(cl-annot:enable-annot-syntax)

@export
(defclass <clack-logger-file> (<clack-logger-stream>)
     ((output-file :type (or string pathname)
                   :initarg :output-file
                   :initform (error ":output-file is required.")
                   :accessor output-file
                   :documentation "Where to output log messages.")))

(defmethod output ((this <clack-logger-file>))
  "Output log messages to 'output-file' in this slot."
  (with-open-file (stream (output-file this)
                          :direction :output
                          :if-exists :append
                          :if-does-not-exist :create)
    (setf (output-stream this) stream)
    (call-next-method)))

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
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Logger.Stream
* Clack.Logger.Base
* Clack.Middleware.Logger
* Clack.Logger
"
