(in-package :cl-user)
(defpackage clack.middleware.clsql
  (:use :cl
        :clack.component
        :clack.middleware)
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
