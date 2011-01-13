#|
  This file is a part of Slinky package.
  URL: http://github.com/fukamachi/slinky
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Slinky is freely distributable under the LLGPL License.
|#

#|
  Core class for Slinky.

  Author: Eitarow Fukamachi (fukamachi_e@ariel-networks.com)
|#

(in-package :slinky.core)

(defclass <collect-metaclass> (standard-class)
     ((instance-collection :initform '() :accessor instance-collection))
  (:documentation "Metaclass to collect instances  automatically."))

#-(or clisp allegro)
(defmethod c2mop:validate-superclass ((class <collect-metaclass>)
                                      (super standard-class))
  t)

(defmethod allocate-instance ((class <collect-metaclass>) &key)
  "Put a new instance into `instance-collection'."
  (let ((instance (call-next-method)))
    (push instance (instance-collection class))
    instance))
