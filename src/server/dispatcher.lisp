#|
  This file is a part of Slinky package.
  URL: http://github.com/fukamachi/slinky
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Slinky is freely distributable under the LLGPL License.
|#

#|
  Dispatcher for Hunchentoot. It dispatches requests to each Slinky action.
  The dispatcher is a function, takes an argument `hunchentoot:*request*'
    and returns a handler.

  Author: Eitarow Fukamachi (fukamachi_e@ariel-networks.com)
|#

(in-package :slinky.server)

(defun create-slinky-dispatcher (app)
  "Return a dispatcher of Hunchentoot. Takes a object `<application>'."
  (lambda (request)
    (funcall (route app)
             (hunchentoot:request-uri request))))
