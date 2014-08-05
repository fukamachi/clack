#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.middleware.session.cookie
  (:use :cl
        :clack.session.state
        :split-sequence)
  (:import-from :clack.middleware.session
                :<clack-middleware-session>
                :state
                :extract
                :save-state
                :commit)
  (:import-from :clack.util
                :hmac-sha1-hex-string)
  (:import-from :marshal
                :marshal
                :unmarshal)
  (:import-from :base64
                :base64-string-to-string
                :string-to-base64-string)
  (:import-from :alexandria
                :hash-table-plist))
(in-package :clack.middleware.session.cookie)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-middleware-session-cookie> (<clack-middleware-session>)
     ((secret :type (or string null)
              :initarg :secret
              :initform nil
              :accessor secret
              :documentation "Server side secret to sign the session data using HMAC SHA1.")))

(defmethod extract ((this <clack-middleware-session-cookie>) env)
  (let ((cookie (session-id (state this) env)))
    (multiple-value-bind (val pos) (split-sequence #\: cookie :count 2)
      (values (or (car val) (generate-id (state this) env))
              (let ((base64 (or (cadr val) ""))
                    (signature (subseq cookie pos)))
                (when (string= (signature this base64)
                               signature)
                  (ignore-errors
                    (unmarshal
                     (read-from-string
                      (base64-string-to-string base64))))))))))

(defgeneric signature (mw base64))

(defmethod signature ((this <clack-middleware-session-cookie>) base64)
  (hmac-sha1-hex-string base64
                        (or (secret this) "")))

(defmethod save-state ((this <clack-middleware-session-cookie>) id res env)
  (let ((cookie (serialize this id (getf env :clack.session))))
    (clack.session.state:finalize
     (state this)
     cookie res
     (hash-table-plist (getf env :clack.session.options)))))

(defmethod commit ((this <clack-middleware-session-cookie>) env)
  ;; do nothing
  )

(defgeneric serialize (mw id session))

(defmethod serialize ((this <clack-middleware-session-cookie>) id session)
  (let ((base64 (string-to-base64-string
                 (prin1-to-string (marshal session)))))
    (format nil "~A:~A:~A"
            id
            base64
            (signature this base64))))

(doc:start)

@doc:NAME "
Clack.Middleware.Session.Cookie - Session middleware that saves session data in the cookie.
"

@doc:SYNOPSIS "
    (clackup (builder
              (<clack-middleware-session-cookie>
               :secret \"secret key here\")
              app)
"

@doc:AUTHOR "
Eitaro Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Middleware.Session
* Clack.Session.State
"
