#|
  This file is a part of Slinky package.
  URL: http://github.com/fukamachi/slinky
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Slinky is freely distributable under the LLGPL License.
|#

#|
  Class of Slinky Middleware.

  Author: Eitarow Fukamachi (fukamachi_e@ariel-networks.com)
|#

(in-package :slinky.middleware)

(defclass <slinky-middleware> () ()
  (:documentation "Base class of Slinky Middleware."))

(defmethod initialize-instance :after ((mw <slinky-middleware>) &rest initargs)
  "Collect new instance of middleware into `*middlewares*'."
  (push mw *middlewares*))

(defmethod before ((mw <slinky-middleware>) request) request)
(defmethod after ((mw <slinky-middleware>) response) response)
