#|
  This file is a part of Slinky package.
  URL: http://github.com/fukamachi/slinky
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Slinky is freely distributable under the LLGPL License.
|#

#|
  Slinky action package.
  "Action" is a hundler for Hunchentoot.

  Author: Eitarow Fukamachi (fukamachi_e@ariel-networks.com)
|#

(in-package :cl-user)

(defpackage slinky.action
  (:documentation "Slinky action package.")
  (:use :cl)
  (:export :defaction
           :find-action
           :invoke
           :<action>
           :*action*
           :*actions*))
