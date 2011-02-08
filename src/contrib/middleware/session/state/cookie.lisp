#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Session.State.Cookie
  Basic cookie-based session state

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(clack.util:namespace clack.session.state.cookie
  (:use :cl
        :alexandria
        :clack.util
        :clack.request
        :clack.response
        :clack.session.state)
  (:shadow :body :finalize)
  (:export :<clack-session-state-cookie>
           :path
           :domain
           :expires
           :secure
           :httponly
           :session-id
           :expire
           :finalize
           :valid-sid-p
           :extract-id
           :generate-id))

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

(defmethod expire ((this <clack-session-state-cookie>)
                              id res &optional options)
  (setf (gethash :expires options) 0)
  (finalize this id res options))

(defmethod session-id ((this <clack-session-state-cookie>) req)
  (let ((r (make-request req)))
    (cookies r (session-key this))))

(defmethod finalize ((this <clack-session-state-cookie>) id res options)
  (set-cookie this id res
              (merge-options this options)))

(defmethod set-cookie ((this <clack-session-state-cookie>) id res options)
  (let ((r (apply #'make-response res)))
    (setf (set-cookies r (session-key this))
          (append `(:value ,id) options))
    (clack.response:finalize r)))
