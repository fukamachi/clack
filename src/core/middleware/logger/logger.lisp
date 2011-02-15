#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack Middleware for logging.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(clack.util:namespace clack.middleware.logger
  (:use :cl
        :clack
        :anaphora)
  (:import-from :clack.logger
                :*logger-output*
                :*logger-format-string*
                :*logger-time-format*
                :*logger-min-level*)
  (:import-from :clack.logger.base :output)
  (:import-from :clack.logger.stream :<clack-logger-stream>))

(cl-annot:enable-annot-syntax)

@export
(defclass <clack-middleware-logger> (<middleware>)
     ((logger :initarg :logger :initform (make-instance '<clack-logger-stream>)
              :accessor logger
              :documentation "Logger instance, inherits `<clack-logger-base>'.
If unspecified, `<clack-logger-stream>' will be used by default, and output logs to `*standard-output*'.
If nil, won't output any logs.")
      (min-level :initarg :min-level :reader min-level
                 :documentation "Minimum log level to output. See `clack.logger:*logger-min-level*'.")
      (time-format :initarg :time-format :reader time-format
                   :documentation "Timestamp format string. See `clack.logger:*logger-time-format*'.")
      (format-string :initarg :format-string :reader format-string
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

(defmethod call ((this <clack-middleware-logger>) req)
  "Output log messages."
  (prog1 (call-next this req)
         (awhen (logger this) (output it))))
