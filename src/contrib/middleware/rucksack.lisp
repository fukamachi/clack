#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.middleware.rucksack
  (:use :cl
        :clack)
  (:import-from :rucksack
                :with-transaction
                :current-rucksack))
(in-package :clack.middleware.rucksack)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-middleware-rucksack> (<middleware>)
     ((rucksack :type rs:rucksack
                :initarg :rucksack
                :initform (current-rucksack)
                :accessor rucksack)))

(defmethod call ((this <clack-middleware-rucksack>) env)
  (with-transaction (:rs (rucksack this))
    (call-next this env)))

(doc:start)

@doc:NAME "
Clack.Middleware.Rucksack - Middleware for Rucksack connection management.
"

@doc:SYNOPSIS "
    (defvar *rs* (rs:open-rucksack \"db/\"))
    
    (builder
     (<clack-middleware-rucksack>
      :rucksack *rs*)
     app)

    ;; also same
    (builder
     <clack-middleware-rucksack>
     app)
"

@doc:AUTHOR "
* Eitaro Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* [Rucksack](https://github.com/arielnetworks/rucksack)
"
