#|
  This file is a part of hatenastamp project.
  Copyright (c) 2012 nitro_idiot (nitro_idiot@hatena.ne.jp)
|#

(clack.util:namespace caveman.middleware.dbi
  (:use :cl
        :clack)
  (:import-from :dbi
                :connect
                :disconnect))

(cl-syntax:use-syntax :annot)

@export
(defvar *db* nil)

@export
(defclass <caveman-middleware-dbi> (<middleware>)
     ((driver-name :initarg :driver-name
                   :accessor driver-name)
      (args :initarg :args
            :accessor args)))

(defmethod call ((this <caveman-middleware-dbi>) env)
  (let ((*db* (apply #'dbi:connect (driver-name this)
                     (loop for (k v) on (args this) by #'cddr
                           when v
                             append (list k v)))))
    (prog1 (call-next this env)
           (dbi:disconnect *db*))))

(doc:start)

@doc:NAME "
Clack.Middleware.Dbi - Middleware for CL-DBI connection management.
"

@doc:SYNOPSIS "
    (builder
     (<clack-middleware-dbi>
      :driver-name :mysql
      :database-name \"dbname\"
      :username \"fukamachi\"
      :password \"password\")
     app)
"

@doc:DESCRIPTION "
This is a Clack Middleware component for managing CL-DBI's connections.

## Slots

* driver-name (Required, Keyword)
* database-name (Required, String)

Other parameters (`:username`, `:password` and so on) will be passed to `dbi:connect` as keyword parameters.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* [CL-DBI](https://github.com/fukamachi/cl-dbi)
"
