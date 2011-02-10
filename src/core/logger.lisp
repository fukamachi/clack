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
           :log-message))

(defvar *logger-output* (make-string-output-stream))
(defvar *logger-format-string* "~A [~:@(~A~)] ~A")
(defvar *logger-time-format*
    '((:DAY 2) #\/ :SHORT-MONTH #\/ (:YEAR 4) #\:
      (:HOUR 2) #\: (:MIN 2) #\: (:SEC 2) #\Space :GMT-OFFSET-OR-Z))

(defun log-message (level message)
  (format *logger-output*
          *logger-format-string*
          (format-timestring nil (now) :format *logger-time-format*)
          level
          message))
