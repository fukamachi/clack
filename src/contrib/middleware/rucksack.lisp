#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.middleware.rucksack
  (:use :cl
        :clack)
  (:import-from :rucksack
                :with-rucksack))

(cl-annot:enable-annot-syntax)

@export
(defclass <clack-middleware-rucksack> (<middleware>)
     ((directory :type (or string pathname)
                          :initarg :directory
                          :accessor database-directory)))

(defmethod call ((this <clack-middleware-rucksack>) req)
  (with-rucksack (rs (database-directory this) :if-exists :supersede)
    (call-next this req)))

(doc:start)

@doc:NAME "
Clack.Middleware.Rucksack - Middleware for Rucksack connection management.
"

@doc:SYNOPSIS "
    (builder
     (<clack-middleware-rucksack>
      :directory \"db/\")
     app)
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* [Rucksack](https://github.com/arielnetworks/rucksack)
"
