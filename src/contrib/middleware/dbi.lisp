#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.middleware.dbi
  (:use :cl
        :clack)
  (:import-from :dbi
                :connect
                :disconnect))
(in-package :clack.middleware.dbi)

(cl-syntax:use-syntax :annot)

@export
(defvar *db* nil)

@export
(defclass <clack-middleware-dbi> (<middleware>)
     ((driver-name :initarg :driver-name
                   :accessor driver-name)
      (connect-args :initarg :connect-args
                    :accessor connect-args)))

(defmethod call ((this <clack-middleware-dbi>) env)
  (let ((*db* (apply #'dbi:connect (cons (driver-name this)
                                         (connect-args this)))))
    (unwind-protect (call-next this env)
      (dbi:disconnect *db*))))

(doc:start)

@doc:NAME "
Clack.Middleware.Dbi - Middleware for CL-DBI connection management.
"

@doc:SYNOPSIS "
    (builder
     (<clack-middleware-dbi>
      :driver-name :mysql
      :connect-args '(:database-name \"dbname\"
                      :username \"fukamachi\"
                      :password \"password\"))
     app)
"

@doc:DESCRIPTION "
This is a Clack Middleware component for managing CL-DBI's connections.

## Slots

* driver-name (Required, Keyword)
* connect-args (Required, List)

`connect-args` parameters (`:username`, `:password` and so on) will be passed to `dbi:connect` as keyword parameters.
"

@doc:AUTHOR "
* Eitaro Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* [CL-DBI](https://github.com/fukamachi/cl-dbi)
"
