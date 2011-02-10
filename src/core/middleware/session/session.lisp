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
        :clack.middleware
        :clack.session.state
        :clack.session.store)
  (:shadow :finalize :expire)
  (:export :<clack-middleware-session>))

#.(rename-package :clack.session.state :clack.session.state '(state))

(defclass <clack-middleware-session> (<middleware>)
     ((state :initarg :state
             :initform
             (make-instance '<clack-session-state>)
             :accessor state)
      (store :initarg :store
             :initform
             (make-instance '<clack-session-store>)
             :accessor store))
  (:documentation "Clack Middleware for session management."))

(defmethod call ((this <clack-middleware-session>) req)
  (multiple-value-bind (id session) (extract this req)
    (setf (getf req :clack.session) session)
    (let ((options (make-hash-table :test #'equal)))
      (setf (gethash :id options) id)
      (setf (getf req :clack.session.options) options))

    (let ((res (call-next this req)))
      (finalize this req res))))

(defmethod extract ((this <clack-middleware-session>) req)
  "Extract session id and state."
  (let* ((id (extract-id (state this) req))
         (session (and id
                       (fetch (store this) id))))
    (values (or id (generate-id (state this) req))
            (or session (make-hash-table :test #'equal)))))

(defmethod finalize ((this <clack-middleware-session>) req res)
  (let ((options (getf req :clack.session.options)))
    (unless (gethash :no-store options)
      (commit this req))
    (if (gethash :expire options)
        (expire this (gethash :id options) res req)
        (save-state this (gethash :id options) res req))))

(defmethod commit ((this <clack-middleware-session>) req)
  (let ((session (getf req :clack.session))
        (options (getf req :clack.session.options)))
    (cond
      ((gethash :expire options)
       (remove-session (store this) (gethash :id session)))
      ((gethash :change-id options)
       (remove-session (store this) (gethash :id session))
       (setf (gethash :id options) (generate-id (state this) req))
       (store-session (store this) (gethash :id options) session))
      (t
       (store-session (store this) (gethash :id options) session)))))

(defmethod expire ((this <clack-middleware-session>) id res req)
  (state:expire
   (state this)
   id res
   (hash-table-plist (getf req :clack.session.options))))

(defmethod save-state ((this <clack-middleware-session>) id res req)
  (state:finalize
   (state this)
   id res
   (hash-table-plist (getf req :clack.session.options))))
