#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.middleware.logger
  (:use :cl
        :clack.component
        :clack.middleware
        :anaphora)
  (:import-from :clack.logger
                :*logger-output-hook*
                :*logger-format-string*
                :*logger-time-format*
                :*logger-min-level*)
  (:import-from :clack.logger.base :output)
  (:import-from :clack.logger.stream :<clack-logger-stream>))

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-middleware-logger> (<middleware>)
     ((logger :type (or function <clack-logger-base>)
              :initarg :logger
              :initform (make-instance '<clack-logger-stream>)
              :accessor logger
              :documentation "Logger instance, inherits `<clack-logger-base>'.
If unspecified, `<clack-logger-stream>' will be used by default, and will output logs to `*standard-output*'.
If nil, won't output any logs.")
      (min-level :type (or keyword null)
                 :initarg :min-level
                 :reader min-level
                 :documentation "Minimum log level to output. See `clack.logger:*logger-min-level*'.")
      (time-format :type (or cons null)
                   :initarg :time-format
                   :reader time-format
                   :documentation "Timestamp format string. See `clack.logger:*logger-time-format*'.")
      (format-string :type (or string null)
                     :initarg :format-string
                     :reader format-string
                     :documentation "Log message format string. See `clack.logger:*logger-format-string*'."))
  (:documentation "Clack Middleware for logging."))

(defmacro setf-if-slot-bound (place object slot)
  "Set slot if the slot is uninitialized."
  `(when (slot-boundp ,object ,slot)
     (setf ,place (slot-value ,object ,slot))))

(defmethod initialize-instance :after ((this <clack-middleware-logger>) &key)
  "Set parameters in 'clack.logger'."
  (setf-if-slot-bound *logger-format-string* this 'format-string)
  (setf-if-slot-bound *logger-time-format* this 'time-format)
  (setf-if-slot-bound *logger-min-level* this 'min-level))

(defmethod call ((this <clack-middleware-logger>) env)
  "Output log messages."
  (let* ((hook *logger-output-hook*)
         (logger-fn (if (functionp (logger this))
                        (logger this)
                        #'(lambda (message) (output (logger this) message))))
         (*logger-output-hook*
          #'(lambda (message)
              (funcall hook message)
              (funcall logger-fn message))))
    (call-next this env)))

(doc:start)

@doc:NAME "
Clack.Middleware.Logger - Clack Middleware for logging.
"

@doc:SYNOPSIS "
    ;; Output log messages.
    (clackup (builder
              <clack-middleware-logger>
              (lambda (env)
                (log-message :notice \"You've got an access!\")
                '(200 nil (\"ok\")))))
    
    ;; Output log messages to other place.
    (clackup (builder
              (<clack-middleware-logger>
               :logger (make-instance '<clack-logger-file>))
              (lambda (env)
                (log-message :notice \"You've got an access!\")
                '(200 nil (\"ok\")))))
"

@doc:DESCRIPTION "
Clack.Middleware.Logger allows you to logg messages without stopping the Lisp process. You can customize the minimum level, format, and output destination of log messages.

Currently, Clack.Middleware.Logger can have only one logger. Sorry for the inconvenience.
"

@doc:AUTHOR "
Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Logger
"
