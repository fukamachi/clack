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

(defclass <slinky-application> ()
  ((name :initarg :name :accessor get-name)
   (middleware :initarg :middleware :initform '())
   (root-dir :initarg :root-dir))
  (:metaclass <collect-metaclass>)
  (:documentation "Class of Slinky application."))

(defmethod initialize-instance :after ((app <slinky-application>) &rest initargs)
  ;; TODO: add builtin middlewares.
  ;; TODO: `make-instance' all middleware.
  )

(defmethod dispatch ((app <slinky-application>) request)
  ;; TODO: invoke `*middlewares*'
  )
