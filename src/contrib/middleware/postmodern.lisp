

(in-package :cl-user)
(defpackage clack.middleware.postmodern
  (:use #:cl
	#:clack)
  (:import-from #:postmodern
		#:with-connection
		#:*database*))
(in-package :clack.middleware.postmodern)


(cl-syntax:use-syntax :annot)

@export
(defclass <clack-middleware-postmodern> (<middleware>)
  ((database :type string
	     :initarg :database
	     :accessor database
	     :documentation "The name of the database to connect to.")
   (user :type string
	 :initarg :user
	 :accessor user
	 :documentation "The name of the database user.")
   (password :type string
	     :initarg :password
	     :accessor password
	     :documentation "The password for the database.")
   (host :type string
	 :initarg :host
	 :initform "localhost"
	 :accessor host
	 :documentation "The host the database server is on.")
   (port :type integer
	 :initarg :port
	 :initform 5432
	 :accessor port
	 :documentation "The port the database server is listening on.")
   (pooled-p :type boolean
	     :initarg :pooled-p
	     :initform t
	     :accessor pooled-p
	     :documentation "Shall the connections be pooled?")
   (use-ssl :type keyword
	    :initarg :use-ssl
	    :initform :no
	    :accessor use-ssl
	    :documentation "Keyword for the use-ssl configuration."))
  (:documentation
   "This middleware opens a connection to the specified database
and stores it in *database* as expected by postmodern. It also
stores the connection(s) in an alist (database name as key) in
the environment with the key :clack.postmodern.databases."))

(defmethod call ((this <clack-middleware-postmodern>) env)
  (with-slots (database
	       user
	       password
	       host
	       port
	       pooled-p
	       use-ssl) this
    (with-connection (list database
			   user
			   password
			   host
			   :port port
			   :pooled-p pooled-p
			   :use-ssl use-ssl)
      (let* ((old-dbs (getf env :databases))
	     (new-dbs (acons database *database* old-dbs)))
	;; store the databases in env
	(setf (getf env :clack.postmodern.databases) new-dbs)
	(let ((res (call-next this env)))
	  ;; reset env to the former state
	  (setf (getf env :clack.postmodern.databases) old-dbs)
	  res)))))

@export 
(defun get-connection (db-name env)
  (cdr (assoc db-name
	      (getf env :clack.postmodern.databases)
	      :test #'string=)))

;;; TODO documentation
(doc:start)

@doc:NAME "
Clack.Middleware.Postmodern - Middleware for Postmodern connection management
"

@doc:SYNOPSIS "
 (builder
  (<clack-middleware-postmodern>
   :database \"database-name\"
   :user \"database-user\"
   :password \"database-password\"
   :host \"remote-address\")
  app)
"

@doc:DESCRIPTION "
This is a Clack Middleware component for managing Postmodern connections.

The database connection will be bound to postmodern:*database* as expected
by most of the postmodern functionality. Additionally it can be found
in the environment under the key :clack.postmodern.databases in form of
an list that maps the database names to the connections. This allows
the connection to multiple databases. The connections can be retrieved
by calling (get-connection db-name env).

## Slots

* database (String)

The name of the database.

* user (String)

The database user name.

* password (String)

The password to the database.

* host (Optional, String)

The address of the database server. The default is \"localhost\"

* port (Optional, Integer)

The port on which the database server is listening.

* pooled-p (Optional, Boolean)

If true, then the connection pool is used. The default is true.

* use-ssl (Optional, Keyword)

Same meaning as in the connection spec to a postmodern database. 
"

@doc:AUTHOR "
* Karl Heinrichmeyer (karl.heinrichmeyer@gmail.com)
"

@doc:SEE "
* [Postmodern](http://marijnhaverbeke.nl/postmodern/)
"
