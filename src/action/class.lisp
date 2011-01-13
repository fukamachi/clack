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

(defclass <slinky-action> ()
     ((name :initarg :name :accessor :get-name)
      (lambda-list :initarg :lambda-list :initform '())
      (body :initarg :body :initform (lambda () "Null action.")))
  (:metaclass <collect-metaclass>)
  (:documentation "Class of Slinky action."))

(defmethod invoke ((action <slinky-action>) request)
  "Invoke action with got `request' context."
  (let ((param-fn (if (eq :GET (request-method request))
                      #'get-parameter
                      #'post-parameter))
        (*action* action))
    (apply (slot-value action 'body)
           (mapcar #'(lambda (name) (funcall param-fn name request))
                   (slot-value action 'lambda-list)))))
