(in-package :cl-user)

#-abcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package '#:clack.response)
    (do-external-symbols (symbol (find-package '#:clack.response))
      (unexport symbol '#:clack.response))))

(defpackage clack.response
  (:use :cl)
  (:import-from :clack.request-response
                :headers)
  (:import-from :trivial-types
                :property-list)
  (:import-from :alexandria
                :ensure-list
                :doplist
                :when-let
                :make-keyword)
  (:import-from :quri
                :url-encode)
  (:import-from :local-time
                :format-timestring
                :universal-to-timestamp
                :+gmt-zone+)
  (:export :status))
(in-package :clack.response)

(cl-syntax:use-syntax :annot)

@export
(defclass <response> ()
     ((status :type (or integer null)
              :initarg :status
              :initform nil
              :accessor status)
      (headers :type property-list
               :initarg :headers
               :initform nil)
      (body :type (or list pathname function (vector (unsigned-byte 8)))
            :initarg :body
            :initform nil
            :reader body)
      (set-cookies :initform nil))
  (:documentation "Portable HTTP Response object for Clack response."))

(defmethod initialize-instance :after ((res <response>) &key)
  (when (stringp (body res))
    (setf (body res)
          `(,(body res)))))

@export
;; constructor
(defun make-response (&optional status headers body)
  "A synonym for (make-instance '<response> ...).
Create a <response> instance."
  (make-instance '<response>
     :status status
     :headers headers
     :body body))

(defun normalize-key (name)
  "Returns a keyword of NAME."
  (etypecase name
    (keyword name)
    ((or string symbol) (make-keyword name))))

@export
(defgeneric headers (res &optional name)
  (:documentation
   "Get all included headers, or the header value that corresponds to `name'.

Example:
  (headers res)
  ;;=> (:content-type \"text/plain\")
  (headers res :content-type)
  ;;=> \"text/plain\"")
  (:method ((res <response>) &optional name)
    (if name
        (getf (headers res) (normalize-key name))
        (slot-value res 'headers))))

@export
(defgeneric (setf headers) (value res &optional name)
  (:documentation
   "Set headers.

Example:
  (setf (headers res) '(:content-type \"text/html\"))
  (setf (headers res :content-type) \"text/html\")")
  (:method (value (res <response>) &optional name)
    (if name
        (setf (getf (slot-value res 'headers) (normalize-key name)) value)
        (setf (slot-value res 'headers) value))))

@export
(defgeneric push-header (res name value)
  (:documentation
   "Push the given header pair into response headers.
Example: (push-header res :content-type \"text/html\")")
  (:method ((res <response>) name value)
    (setf (headers res)
          `(,name ,value ,@(headers res)))))

@export
(defgeneric (setf body) (value res)
  (:documentation
   "Set body with normalizing. body must be a list.")
  (:method (value (res <response>))
    (setf (slot-value res 'body)
          (etypecase value
            (string (ensure-list value))
            ((or list pathname function (vector (unsigned-byte 8))) value)))))

@export
(defgeneric set-cookies (res &optional name)
  (:documentation
   "Get whole of set-cookies plist or the set-cookie value of given name.

Example:
  (set-cookies res)
  ;;=> (:hoge (:value \"1\"))
  (set-cookies res :hoge)
  ;;=> (:value \"1\")")
  (:method ((res <response>) &optional name)
    (let ((cookies (slot-value res 'set-cookies)))
      (if name
          (getf cookies (normalize-key name))
          cookies))))

@export
(defgeneric (setf set-cookies) (value res &optional name)
  (:documentation
   "Set set-cookies.

Example:
  (setf (set-cookies res) '(:hoge (:value \"1\")))
  (setf (set-cookies res :hoge) '(:value \"1\" :secure t :httponly t))")
  (:method (value (res <response>) &optional name)
    (if name
        (setf (getf (slot-value res 'set-cookies) (normalize-key name))
              (if (consp value)
                  value
                  `(:value ,value)))
        (setf (slot-value res 'set-cookies) value))))

@export
(defgeneric redirect (res url &optional status)
  (:documentation
   "Set headers for redirecting to given url.")
  (:method ((res <response>) url &optional (status 302))
    (setf (headers res :location) url)
    (setf (status res) status)
    url))

@export
(defgeneric finalize (res)
  (:documentation
   "Return a Clack response list containing three values: status, headers, and body.")
  (:method ((res <response>))
    (finalize-cookies res)
    (list (status res)
          (headers res)
          (body res))))

(defun finalize-cookies (res)
  "Convert set-cookies into a header pair and push it into headers."
  (check-type res <response>)
  (doplist (k v (set-cookies res))
    (push-header res :set-cookie (bake-cookie res k v))))

(defun bake-cookie (res k v)
  "Create a string for Set-Cookie of the request header."
  (check-type res <response>)

  (unless v (return-from bake-cookie ""))

  (let ((cookie `((,(quri:url-encode (symbol-name k))
                   ,(quri:url-encode (getf v :value))))))
    (when-let (domain (getf v :domain))
      (push `("domain" ,domain) cookie))
    (when-let (path (getf v :path))
      (push `("path" ,path) cookie))
    (when-let (expires (getf v :expires))
      (push `("expires"
              ,(format-timestring
                nil (universal-to-timestamp expires)
                :format '(:short-weekday ", " (:day 2) #\  :short-month #\  (:year 4) #\  (:hour 2) #\:
                          (:min 2) #\: (:sec 2) " GMT")
                :timezone +gmt-zone+)) cookie))
    (when (getf v :secure)
      (push '("secure") cookie))
    (when (getf v :httponly)
      (push '("HttpOnly") cookie))

    (format nil
            "~{~{~A~^=~}~^; ~}"
            (nreverse cookie))))
