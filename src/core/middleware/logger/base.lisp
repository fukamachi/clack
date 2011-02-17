#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
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

(doc:start)

@doc:NAME "
Clack.Logger.Base - Base class for Clack loggers.
"

@doc:DESCRIPTION "
Clack.Logger.Base is base class for Clack loggers to pass to Clack.Middleware.Logger. All you have to do is to inherit `<clack-logger-base>' and then implement a method `output'.
"

@doc:AUTHOR "
Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Middleware.Logger
* Clack.Logger.Stream
"
