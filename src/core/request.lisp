#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.request
  (:use :cl
        :anaphora
        :metabang-bind)
  (:import-from :trivial-types
                :property-list)
  (:import-from :alexandria
                :when-let
                :make-keyword)
  (:import-from :flexi-streams
                :make-external-format
                :make-flexi-stream)
  (:import-from :cl-ppcre
                :split
                :scan-to-strings
                :register-groups-bind)
  (:import-from :clack.util
                :getf-all
                :merge-plist
                :nappend)
  (:import-from :clack.util.stream
                :ensure-character-input-stream
                :make-replay-buffer
                :make-replay-input-stream)
  (:export :request-method
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
           :uploads))

(cl-syntax:use-syntax :annot)

@export
(defclass <request> ()
     ((request-method :type keyword
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
This may be an empty string if the request URL targets the application root and does no have a trailing slash.")
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

      (http-referer :type (or string null)
                    :initarg :http-referer
                    :initform nil
                    :reader referer)
      (http-user-agent :type (or string null)
                       :initarg :http-user-agent
                       :initform nil
                       :reader user-agent)
      (http-cookie :type (or string list)
                   :initarg :http-cookie
                   :initform nil)

      (body-parameters :type property-list
                       :initform nil)
      (query-parameters :type property-list
                        :initform nil)
      (uploads :type list
               :initarg :clack.uploads
               :initform nil
               :accessor uploads))
  (:documentation "Portable HTTP Request object for Clack Request."))

(defmethod initialize-instance :after ((this <request>) &key)
  ;; cookies
  (swhen (slot-value this 'http-cookie)
    (setf it (parameters->plist it :delimiter "\\s*[,;]\\s*")))

  ;; GET parameters
  (setf (slot-value this 'query-parameters)
        (parameters->plist (query-string this)))

  ;; POST parameters
  (bind ((body (raw-body this))
         ((:values type subtype charset)
          (parse-content-type (content-type this)))
         (content-type (concatenate 'string type "/" subtype))
         (external-format
          (flex:make-external-format
           (if charset
               (make-keyword (string-upcase charset))
               :utf-8)
           :eol-style :lf)))
    (cond
      ((string= content-type "application/x-www-form-urlencoded")
       (setf (slot-value this 'body-parameters)
             (parameters->plist (read-line (ensure-character-input-stream body) nil ""))))
      ((and (string= content-type "multipart/form-data")
            (not (uploads this))) ;; not set yet.
       (setf (uploads this)
             (clack.util.hunchentoot:parse-rfc2388-form-data
              (flex:make-flexi-stream body)
              content-type
              external-format))))))

@export
(defun shared-raw-body (env)
  "Returns a shared raw-body, or returns nil if raw-body is
empty. This function modifies REQ to share raw-body among the
instances of <request>."
  (when-let ((body (getf env :raw-body)))
    (let ((buffer (getf env :raw-body-buffer)))
      (unless buffer
        ;; Raw-body is fresh and nothing has been read.
        (setf buffer (make-replay-buffer))
        (nappend env `(:raw-body-buffer ,buffer)))
      (make-replay-input-stream body :buffer buffer))))

@export
;; constructor
(defun make-request (env)
  "A synonym for (make-instance '<request> ...).
Make a <request> instance from environment plist. Raw-body of the instance
will be shared, meaning making an instance of <request> doesn't effect
on an original raw-body."
  (apply #'make-instance '<request>
         :allow-other-keys t
         :raw-body (shared-raw-body env)
         env))

@export
(defmethod securep ((req <request>))
  (eq (uri-scheme req) :https))

@export
(defmethod cookies ((req <request>) &optional name)
  "Returns cookies as a plist. If optional `name' is specified, returns the value corresponds to it."
  (get-whole-or-specified req 'http-cookie name))

@export
(defmethod body-parameter ((req <request>) &optional name)
  "Return POST parameters as a plist. If optional `name' is specified, returns the value corresponds to it."
  (get-whole-or-specified req 'body-parameters name))

@export
(defmethod query-parameter ((req <request>) &optional name)
  "Returns GET parameters as a plist. If optional `name' is specified, returns the value corresponds to it."
  (get-whole-or-specified req 'query-parameters name))

@export
(defmethod parameter ((req <request>) &optional name)
  "Returns request parameters containing (merged) GET and POST parameters. If optional `name' is specified, returns the value corresponds to it."
  (let ((params (merge-plist (query-parameter req)
                             (body-parameter req))))
    (if name
        (getf-all params name)
        params)))

(defmethod get-whole-or-specified ((req <request>) key &optional name)
  (let ((params (slot-value req key)))
    (if name
        (getf-all params name)
        params)))

(defun parameters->plist (params &key (delimiter "&"))
  "Convert parameters into plist. The `params' must be a string."
  (loop for kv in (ppcre:split delimiter params)
        for (k v) = (ppcre:split "=" kv)
        append (list (make-keyword k)
                     ;; KLUDGE: calls `ignore-errors'.
                     (or (ignore-errors (clack.util.hunchentoot:url-decode v)) v))))

(defun parse-content-type (content-type)
  "Parse Content-Type from Request header."
  (ppcre:register-groups-bind (type subtype params)
      ("^(.+?)/([^;]+);?(?:(.+)|$)" content-type)
    (values
     (or type "application")
     (or subtype "octet-stream")
     (when params
       (aand (nth-value 1 (ppcre:scan-to-strings
                           "charset=([^; ]+)" params))
             (aref it 0))))))

(doc:start)

@doc:NAME "
Clack.Request - Portable HTTP Request object for Clack Request.
"

@doc:SYNOPSIS "
    (defun app (env)
      (let ((req (make-request env)))
      `(200
        (:content-type \"text/plain\")
        (\"Hello, \" (query-parameter req \"name\")))))
"

@doc:DESCRIPTION "
Clack.Request provides a consistent API for request objects.
"

@doc:AUTHOR "
* Eitarow Fukamachi (e.arrows@gmail.com)
"
