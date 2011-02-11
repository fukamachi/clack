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
  (:use :cl)
  (:import-from :clack.middleware
                :<middleware>
                :call)
  (:import-from :clack.logger
                :*logger-output*
                :*logger-format-string*
                :*logger-time-format*
                :*logger-min-level*)
  (:export :<clack-middleware-logger>))

(defclass <clack-middleware-logger> (<middleware>)
     ((output-stream :initarg :output-stream
       :reader output-stream)
      (min-level :initarg :min-level
       :reader min-level)
      (time-format :initarg :time-format
       :reader time-format)
      (format-string :initarg :format-string
       :reader format-string))
  (:documentation "Clack Middleware for logging."))

(defun setf-if-slot-bound (place object slot)
  (when (slot-boundp object slot)
    (setf place (slot-value object slot))))

(defmethod initialize-instance :after ((this <clack-middleware-logger>)
                                       &rest initargs)
  (declare (ignore initargs))
  (setf-if-slot-bound *logger-output* this 'output-stream)
  (setf-if-slot-bound *logger-format-string* this 'format-string)
  (setf-if-slot-bound *logger-time-format* this 'time-format)
  (setf-if-slot-bound *logger-min-level* this 'min-level))
