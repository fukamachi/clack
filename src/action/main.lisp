#|
  This file is a part of Slinky package.
  URL: http://github.com/fukamachi/slinky
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Slinky is freely distributable under the LLGPL License.
|#

#|
  Functions about Slinky action.

  Author: Eitarow Fukamachi (fukamachi_e@ariel-networks.com)
|#

(in-package :slinky.action)

(defmacro find-action (name &key (test #'eq))
  "Find `<slinky-action>' by it's name and return the instance."
  (find name (instance-collection (find-class '<slinky-action>))
        :key #'get-name :test test))
