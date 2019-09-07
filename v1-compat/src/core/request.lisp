(in-package :cl-user)

#-abcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package '#:clack.request)
    (do-external-symbols (symbol (find-package '#:clack.request))
      (unexport symbol '#:clack.request))))

(defpackage clack.request
  (:use :cl)
  (:import-from :clack.request-response
                :headers)
  (:import-from :trivial-types
                :association-list
                :property-list)
  (:import-from :http-body
                :parse)
  (:import-from :quri
                :url-decode-params)
  (:import-from :alexandria
                :when-let)
  (:import-from :flexi-streams
                :make-external-format
                :make-flexi-stream)
  (:import-from :cl-ppcre
                :split
                :scan-to-strings
                :register-groups-bind)
  (:import-from :circular-streams
                :make-circular-input-stream
                :circular-stream-buffer)
  (:export :env
           :request-method
           :script-name
           :path-info
           :server-name
           :server-port
           :server-protocol
           :request-uri
           :uri-scheme
           :remote-addr
           :remote-port
           :query-string
           :raw-body
           :content-length
           :content-type
           :clack-handler

           :referer
           :user-agent
           :headers))
(in-package :clack.request)

(cl-syntax:use-syntax :annot)

@export
(defclass <request> ()
     ((env :type property-list
           :initarg :env
           :reader env
           :documentation "Raw env")
      (request-method :type keyword
                      :initarg :request-method
                      :reader request-method
                      :documentation "The HTTP request method.
This must be one of :GET, :HEAD, :OPTIONS, :PUT, :POST, or :DELETE.")
      (script-name :type string
                   :initarg :script-name
                   :reader script-name
                   :documentation "The initial portion of the request URL's path, corresponding to the application.
This may be an empty string if the application corresponds to the server's root URI. If this key is not empty, it must start with a forward slash (/).")
      (path-info :type string
                 :initarg :path-info
                 :reader path-info
                 :documentation "The remainder of the request URL's path.
This may be an empty string if the request URL targets the application root and does not have a trailing slash.")
      (server-name :type string
                   :initarg :server-name
                   :reader server-name
                   :documentation "The resolved server name, or the server IP address.")
      (server-port :type integer
                   :initarg :server-port
                   :reader server-port
                   :documentation "The port on which the request is being handled.")
      (server-protocol :type keyword
                       :initarg :server-protocol
                       :reader server-protocol
                       :documentation "The version of the protocol the client used to send the request.
Typically this will be something like :HTTP/1.0 or :HTTP/1.1.")
      (request-uri :type string
                   :initarg :request-uri
                   :reader request-uri
                   :documentation "The request URI. Must start with '/'.")
      (uri-scheme :type keyword
                  :initarg :uri-scheme
                  :initform :http
                  :reader uri-scheme)
      (remote-addr :type string
                   :initarg :remote-addr
                   :reader remote-addr)
      (remote-port :type integer
                   :initarg :remote-port
                   :reader remote-port)
      (query-string :type (or string null)
                    :initarg :query-string
                    :initform nil
                    :reader query-string
                    :documentation "The portion of the request URL that follows the '?', if any.")
      (raw-body :type (or stream null)
                :initarg :raw-body
                :initform nil
                :reader raw-body)
      (content-length :type (or integer null)
                      :initarg :content-length
                      :initform nil
                      :reader content-length)
      (content-type :type (or string null)
                    :initarg :content-type
                    :initform nil
                    :reader content-type)
      (clack-handler :type keyword
                     :initarg :clack-handler
                     :reader clack-handler)

      (headers :type hash-table
               :initarg :headers
               :initform (make-hash-table :test 'equal))

      (http-cookie :type (or string list)
                   :initarg :http-cookie
                   :initform nil)

      (body-parameters :type association-list
                       :initarg :body-parameters)
      (query-parameters :type association-list
                        :initarg :query-parameters))
  (:documentation "Portable HTTP Request object for Clack Request."))

(defgeneric referer (request)
  (:method ((request <request>))
    (gethash "referer" (headers request))))

(defgeneric user-agent (request)
  (:method ((request <request>))
    (gethash "user-agent" (headers request))))

(defmethod headers ((request <request>) &optional name)
  (if name
      (gethash name (slot-value request 'headers))
      (slot-value request 'headers)))

@export
;; constructor
(defun make-request (env)
  "A synonym for (make-instance '<request> ...).
Make a <request> instance from environment plist. Raw-body of the instance
will be shared, meaning making an instance of <request> doesn't effect
on an original raw-body."
  (let ((req (apply #'make-instance '<request>
                    :env env
                    :allow-other-keys t
                    env)))

    ;; cookies
    (when-let (cookie (gethash "cookie" (headers req)))
      (setf (slot-value req 'http-cookie)
            (loop for kv in (ppcre:split "\\s*[,;]\\s*" cookie)
                  append (quri:url-decode-params kv :lenient t)))
      (rplacd (last env) (list :cookies (slot-value req 'http-cookie))))

    ;; GET parameters
    (unless (slot-boundp req 'query-parameters)
      (setf (slot-value req 'query-parameters)
            (and (query-string req)
                 (quri:url-decode-params (query-string req) :lenient t)))
      (rplacd (last env) (list :query-parameters (slot-value req 'query-parameters))))

    (when (raw-body req)
      (setf (slot-value req 'raw-body)
            (make-circular-input-stream (raw-body req)))

      (setf (getf env :raw-body) (slot-value req 'raw-body))
      ;; POST parameters
      (unless (or (null (content-type req))
                  (slot-boundp req 'body-parameters))
        (setf (slot-value req 'body-parameters)
              (parse (content-type req) (content-length req) (raw-body req)))
        (file-position (raw-body req) 0)
        (rplacd (last env) (list :body-parameters (slot-value req 'body-parameters)))))

    req))

@export
(defgeneric securep (req)
  (:method ((req <request>))
    (eq (uri-scheme req) :https)))

@export
(defgeneric cookies (req &optional name)
  (:documentation
   "Returns cookies as a plist. If optional `name' is specified, returns the value that corresponds to it.")
  (:method ((req <request>) &optional name)
    (get-whole-or-specified req 'http-cookie name)))

@export
(defgeneric body-parameter (req &optional name)
  (:documentation
   "Return POST parameters as a plist. If optional `name' is specified, returns the value that corresponds to it.")
  (:method ((req <request>) &optional name)
    (get-whole-or-specified req 'body-parameters name)))

@export
(defgeneric query-parameter (req &optional name)
  (:documentation
   "Returns GET parameters as a plist. If optional `name' is specified, returns the value that corresponds to it.")
  (:method ((req <request>) &optional name)
    (get-whole-or-specified req 'query-parameters name)))

(defun assoc-value-multi (key params)
  (let ((conses
          (loop for (k . v) in params
                when (string= k key)
                  collect v)))
    (if (null (cdr conses))
        (car conses)
        conses)))

@export
(defgeneric parameter (req &optional name)
  (:documentation
   "Returns request parameters containing (merged) GET and POST parameters. If optional `name' is specified, returns the value that corresponds to it.")
  (:method ((req <request>) &optional name)
    (let ((params (append (query-parameter req)
                          (body-parameter req))))
      (if name
          (assoc-value-multi name params)
          params))))

(defun get-whole-or-specified (req key &optional name)
  (check-type req <request>)
  (let ((params (slot-value req key)))
    (if name
        (assoc-value-multi name params)
        params)))
