#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Logger.Base
  Base class for Clack loggers.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(clack.util:namespace clack.logger.base
  (:use :cl))

(cl-annot:enable-annot-syntax)

@export
(defclass <clack-logger-base> () ()
  (:documentation "Base class for Clack loggers."))

@export
(defgeneric output (logger)
  (:documentation "Output log messages by each loggers' way. All logger must implement this method."))
