

(in-package :cl-user)
(defpackage clack.middleware.postmodern
  (:use #:cl
	#:clack.component
  #:clack.middleware)
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
      (let* ((old-dbs (getf env :clack.postmodern.databases))
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
