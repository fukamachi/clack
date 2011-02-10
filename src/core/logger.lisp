#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Logging utility for Clack.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(clack.util:namespace clack.logger
  (:use :cl)
  (:import-from :local-time
                :now
                :format-timestring)
  (:export :*logger-output*
           :*logger-time-format*
           :*logger-min-level*
           :log-message

           :+debug+
           :+info+
           :+notice+
           :+warning+
           :+error+
           :+critical+
           :+alert+
           :+emergency+))

(defvar *logger-output* (make-string-output-stream)
  "Output stream for logger.
To get all log messages as one string: (get-output-stream-string *logger-output*)")
(defvar *logger-time-format*
    '((:DAY 2) #\/ :SHORT-MONTH #\/ (:YEAR 4) #\:
      (:HOUR 2) #\: (:MIN 2) #\: (:SEC 2) #\Space :GMT-OFFSET-OR-Z)
  "Format list of timestamp in log messages. This is same as LOCAL-TIME.
Default:
  11/Feb/2011:03:37:39 +09:00")
(defvar *logger-format-string* "~A [~:@(~A~)] ~A"
  "Log format string for cl:format.
Example:
  11/Feb/2011:03:37:39 +09:00 [CRITICAL] Help me!!")

(defconstant +debug+ 0)
(defconstant +info+ 1)
(defconstant +notice+ 2)
(defconstant +warning+ 3)
(defconstant +error+ 4)
(defconstant +critical+ 5)
(defconstant +alert+ 6)
(defconstant +emergency+ 7)
(defvar *logger-min-level* +warning+)

(defun log-message (level message)
  "Output a log if the log level is more than `*logger-min-level*'.
Log level must be a integer 0-7, or a keyword represents log level.

Example:
  (log-message :warning \"Something wrong.\")"
  (when (>= (normalize-loglevel level) *logger-min-level*)
    (format *logger-output*
            *logger-format-string*
            (format-timestring nil (now) :format *logger-time-format*)
            level
            message)))

(defun normalize-loglevel (level)
  "Log level is an integer or a keyword."
  (etypecase level
    (integer level)
    (keyword (keyword->loglevel level))))

(defun keyword->loglevel (key)
  "Convert keyword represents log level into a number.
Example:
  (keyword->loglevel :notice \"Hello, Clack!\")
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
