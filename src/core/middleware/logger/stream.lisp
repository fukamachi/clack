#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Logger.Stream
  Output log messages to stream.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(clack.util:namespace clack.logger.stream
  (:use :cl)
  (:import-from :anaphora :awhen :it)
  (:import-from :clack.logger :*logger-output*)
  (:import-from :clack.logger.base :<clack-logger-base> :output)
  (:export :<clack-logger-stream>))

(defclass <clack-logger-stream> (<clack-logger-base>)
     ((output-stream :initarg :output-stream :initform *standard-output*
                     :reader output-stream)))

(defmethod output ((this <clack-logger-stream>))
  "Output log messages to 'output-stream' in this slot."
  (awhen (get-output-stream-string *logger-output*)
    (write-string it (output-stream this))))
