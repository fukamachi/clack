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
   (main :initarg :main
         :initform (lambda (request)
                     (declare (ignore request))
                     "Nil action")
         :accessor get-main)
   (middleware :initarg :middleware :initform '() :accessor get-middleware)
   (root-dir :initarg :root-dir))
  (:metaclass <collect-metaclass>)
  (:documentation "Class of Slinky application."))

(defmethod initialize-instance :after ((app <slinky-application>) &rest initargs)
  (mapcar #'make-instance (get-middleware app)))

(defmethod call ((app <slinky-application>) request)
  "Returns a handler function."
  (setf request
        (reduce #'before *middlewares*
                :initial-value request
                :from-end t))
  (lambda ()
    (reduce #'after (reverse *middlewares*)
            :initial-value (funcall (get-main app) request)
            :from-end t)))
