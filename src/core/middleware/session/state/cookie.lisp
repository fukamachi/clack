#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.session.state.cookie
  (:use :cl
        :clack.session.state)
  (:import-from :clack.request
                :make-request
                :cookies)
  (:import-from :clack.util
                :merge-plist)
  (:import-from :alexandria
                :remove-from-plist)
  (:import-from :clack.response
                :make-response
                :set-cookies)
  (:export :path
           :domain
           :expires
           :secure
           :httponly

           ;; imported from clack.session.state
           :valid-sid-p
           :extract-id
           :generate-id))

(cl-annot:enable-annot-syntax)

@export
(defclass <clack-session-state-cookie> (<clack-session-state>)
     ((path :initarg :path :initform "/" :accessor path)
      (domain :initarg :domain :initform nil :accessor domain)
      (expires :initarg :expires :initform (get-universal-time) :accessor expires)
      (secure :initarg :secure :initform nil :accessor secure)
      (httponly :initarg :httponly :initform nil :accessor httponly)))

(defmethod merge-options ((this <clack-session-state-cookie>) options)
  (setf options (remove-from-plist options :id))
  (merge-plist
   (list
    :path (path this)
    :domain (domain this)
    :secure (secure this)
    :httponly (httponly this)
    :expires (+ (get-universal-time) (expires this)))
   options))

@export
(defmethod expire ((this <clack-session-state-cookie>)
                              id res &optional options)
  (setf (gethash :expires options) 0)
  (finalize this id res options))

@export
(defmethod session-id ((this <clack-session-state-cookie>) req)
  (let ((r (make-request req)))
    (cookies r (session-key this))))

@export
(defmethod finalize ((this <clack-session-state-cookie>) id res options)
  (set-cookie this id res
              (merge-options this options)))

(defmethod set-cookie ((this <clack-session-state-cookie>) id res options)
  (let ((r (apply #'make-response res)))
    (setf (set-cookies r (session-key this))
          (append `(:value ,id) options))
    (clack.response:finalize r)))

(doc:start)

@doc:NAME "
Clack.Session.State.Cookie - Basic cookie-based session state.
"

@doc:DESCRIPTION "
Clack.Session.State.Cookie will maintain session state using browser cookies.
"

@doc:AUTHOR "
Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Session.State
* Clack.Middleware.Session
"
