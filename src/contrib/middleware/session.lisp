#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Middleware.Session.
  Middleware for session management.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(clack.util:namespace clack.middleware.session
  (:use :cl
        :alexandria
        :clack.util
        :clack.middleware
        :clack.session.state
        :clack.session.store)
  (:export :<clack-middleware-session>))

(enable-duck-reader)

(defclass <clack-middleware-session> (<middleware>)
     ((state :initarg :state
             :initform
             (make-instance '<clack-session-state>)
             :accessor state)
      (store :initarg :store
             :initform
             (make-instance '<clack-session-store>)
             :accessor store)))

(defmethod call ((this <clack-middleware-session>) req)
  (multiple-value-bind (id session) (session this req)
    (setf (getf req :clack.session) session)
    (let ((options (make-hash-table :test #'equal)))
      (setf (gethash :id options) id)
      (setf (getf req :clack.session.options) options))

    (let ((res (call-next this req)))
      (finalize-session this req res))))

(defmethod session ((this <clack-middleware-session>) req)
  (let* ((id (_extract (state this) req))
         (session (and id (_fetch (store this) id))))
    (values (or id (generate-id this req))
            (or session (make-hash-table :test #'equal)))))

(defmethod generate-id ((this <clack-middleware-session>) req)
  (_generate (state this) req))

(defmethod finalize-session ((this <clack-middleware-session>) req res)
  (let ((options (getf req :clack.session.options)))
    (unless (gethash :no-store options)
      (commit this req))
    (if (gethash :expire options)
        (expire-session this (gethash :id options) res req)
        (save-state this (gethash :id options) res req))))

(defmethod commit ((this <clack-middleware-session>) req)
  (let ((session (getf req :clack.session))
        (options (getf req :clack.session.options)))
    (cond
      ((gethash :expire options)
       (_remove-session (store this) (gethash :id session)))
      ((gethash :change-id options)
       (_remove-session (store this) (gethash :id session))
       (setf (gethash :id options) (generate-id this req))
       (_store-session (store this) (gethash :id options) session))
      (t
       (_store-session (store this) (gethash :id options) session)))))

(defmethod expire-session ((this <clack-middleware-session>) id res req)
  (_expire-session-id (state this)
                      id res
                      (hash-table-plist (getf req :clack.session.options))))

(defmethod save-state ((this <clack-middleware-session>) id res req)
  (_finalize (state this)
     id res
     (hash-table-plist (getf req :clack.session.options))))
