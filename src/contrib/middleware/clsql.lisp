#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.middleware.clsql
  (:use :cl
        :clack)
  (:import-from :clsql
                :connect
                :disconnect))

(cl-annot:enable-annot-syntax)

@export
(defclass <clack-middleware-clsql> (<middleware>)
     ((database-type :initarg :database-type :initform :sqlite3
                     :accessor database-type)
      (connection-spec :initarg :connection-spec :initform '("memory")
                       :accessor connection-spec)
      (connect-args :initarg :connect-args :initform nil
                    :accessor connect-args)))

(defmethod call ((this <clack-middleware-clsql>) req)
  (prog2
    (apply #'connect
           (connection-spec this)
           :database-type (database-type this)
           (connect-args this))
    (call-next this req)
    (disconnect)))

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
* Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* [CLSQL](http://clsql.b9.com/)
"
