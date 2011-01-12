#|
  This file is a part of Slinky package.
  URL: http://github.com/fukamachi/slinky
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Slinky is freely distributable under the LLGPL License.
|#

#|
  Class of Slinky Application.

  Author: Eitarow Fukamachi (fukamachi_e@ariel-networks.com)
|#

(in-package :slinky.application)

(defclass <application> ()
  ((name :initarg :name :accessor :get-name)
   (root-dir :initarg :root-dir)
   (view-dir :initarg :view-dir :initform *default-view-dir*)
   (action-dir :initarg :action-dir :initform *default-action-dir*)
   (model-dir :initarg :model-dir :initform *default-model-dir*))
  (:documentation "Class of Slinky application."))
