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
   (route :accessor :route)
   (root-dir :initarg :root-dir)
   (view-dir :initarg :view-dir :initform *default-view-dir*)
   (action-dir :initarg :action-dir :initform *default-action-dir*)
   (model-dir :initarg :model-dir :initform *default-model-dir*))
  (:documentation "Class of Slinky application."))

(defun make-routing (routing)
  "Create a function for URL routing and return it.
The function takes HTTP Request method and URI string."
  (loop :with hash = (make-hash-table :test 'equal)
        :for (uri-rule action-name method) :in routing
        ;; TODO: about `method' is nil or cons.
        :do (push (lambda (uri) (and (string= uri-rule uri) action-name))
                  (gethash method hash))
        :finally
        (return (lambda (method uri) (funcall (gethash method hash) uri)))))

(defmethod initialize ((app <application>) (routing (cons)))
  ;; TODO: ...load controller, view and i18n files...
  (setf (route app) (make-routing routing)))
