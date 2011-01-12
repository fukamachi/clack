#|
  This file is a part of Slinky package.
  URL: http://github.com/fukamachi/slinky
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Slinky is freely distributable under the LLGPL License.
|#

#|
  Class of Slinky action.

  Author: Eitarow Fukamachi (fukamachi_e@ariel-networks.com)
|#

(in-package :slinky.action)

(defclass <action> ()
     ((name :initarg :name :accessor :get-name)
      (params-list :initarg :params-list :initform '()))
  (:documentation "Class of Slinky action."))
