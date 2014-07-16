#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.middleware.clsql
  (:use :cl
        :clack)
  (:import-from :clsql
                :connect
                :disconnect))
(in-package :clack.middleware.clsql)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-middleware-clsql> (<middleware>)
     ((database-type :type keyword
                     :initarg :database-type
                     :initform :sqlite3
                     :accessor database-type)
      (connection-spec :type list
                       :initarg :connection-spec
                       :initform '("memory")
                       :accessor connection-spec)
      (connect-args :type list
                    :initarg :connect-args
                    :initform nil
                    :accessor connect-args)))

(defmethod call ((this <clack-middleware-clsql>) env)
  (let* ((db (apply #'connect
                    (connection-spec this)
                    :if-exists :new
                    :make-default nil
                    :database-type (database-type this)
                    (connect-args this)))
         (clsql:*default-database* db))
    (unwind-protect
        (call-next this env)
      (disconnect :database db))))

(doc:start)

@doc:NAME "
Clack.Middleware.Clsql - Middleware for CLSQL connection management.
"

@doc:SYNOPSIS "
    (builder
     (<clack-middleware-clsql>
      :database-type :mysql
      :connection-spec '(\"localhost\" \"db\" \"fukamachi\" \"password\"))
     app)
"

@doc:DESCRIPTION "
This is a Clack Middleware component for managing CLSQL's connection.

## Slots

* database-type (Optional, Keyword)

The default is `:sqlite3`.

* connection-spec (Optional, List)

The default is `(\"memory\")`.

* connect-args (Optional, List)
"

@doc:AUTHOR "
* Eitaro Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* [CLSQL](http://clsql.b9.com/)
"
