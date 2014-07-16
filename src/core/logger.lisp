#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.logger
  (:use :cl
        :cl-annot.doc)
  (:import-from :local-time
                :format-timestring)
  (:import-from :clack.util.localtime
                :now))
(in-package :clack.logger)

(cl-syntax:use-syntax :annot)

@export
(defvar *logger-output* (make-string-output-stream)
  "DEPRECATED: use *logger-output-hook* for instead.
Output broadcast stream for loggers.")

@export
(defvar *logger-output-hook* #'(lambda (message) message)
  "Callback function which will be invoked when logging.")

@export
(defvar *logger-time-format*
    '((:DAY 2) #\/ :SHORT-MONTH #\/ (:YEAR 4) #\:
      (:HOUR 2) #\: (:MIN 2) #\: (:SEC 2) #\Space :GMT-OFFSET-OR-Z)
  "Format list of timestamp in log messages. This is the same as LOCAL-TIME.
Default:
  11/Feb/2011:03:37:39 +09:00")

(defvar *logger-format-string* "~&~A [~:@(~A~)] ~A~%"
  "Log format string for cl:format.
Example:
  11/Feb/2011:03:37:39 +09:00 [CRITICAL] Help me!!")

@export (defconstant +debug+ 0)
@export (defconstant +info+ 1)
@export (defconstant +notice+ 2)
@export (defconstant +warning+ 3)
@export (defconstant +error+ 4)
@export (defconstant +critical+ 5)
@export (defconstant +alert+ 6)
@export (defconstant +emergency+ 7)
@export
(defvar *logger-min-level* +warning+)

@doc "
Output a log if the log level is more than `*logger-min-level*'.
Log level must be a integer 0-7, or a keyword represents log level.

Example:
  (log-message :warning \"Something wrong.\")"
@export
(defun log-message (level format-control &rest format-args)
  @type string format-control
  (when (>= (normalize-loglevel level) *logger-min-level*)
    (let ((message
           (format nil
                   *logger-format-string*
                   (format-timestring nil (now) :format *logger-time-format*)
                   level
                   (apply #'format nil format-control format-args))))
      (write-string message *logger-output*)
      (funcall *logger-output-hook* message))))

(defun normalize-loglevel (level)
  "Log level is an integer or a keyword."
  (etypecase level
    (integer level)
    (keyword (keyword->loglevel level))))

(defun keyword->loglevel (key)
  "Convert a keyword that represents a log level into a number.
Example:
  (keyword->loglevel :notice)
  ;;=> 2"
  (ccase key
    (:debug +debug+)
    (:info +info+)
    (:notice +notice+)
    (:warning +warning+)
    (:error +error+)
    (:critical +critical+)
    (:alert +alert+)
    (:emergency +emergency+)))

(doc:start)

@doc:NAME "
Clack.Logger - Logging utility for Clack.
"

@doc:AUTHOR "
Eitaro Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Middleware.Logger
"
