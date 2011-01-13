#|
  This file is a part of Slinky package.
  URL: http://github.com/fukamachi/slinky
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Slinky is freely distributable under the LLGPL License.
|#

#|
  Class for Middleware to route.

  Author: Eitarow Fukamachi (fukamachi_e@ariel-networks.com)
|#

(in-package :slinky.middleware.route)

(defclass <slinky-middleware-route> (<slinky-middleware>)
     ()
  (:documentation "Slinky Middleware for routing request."))
