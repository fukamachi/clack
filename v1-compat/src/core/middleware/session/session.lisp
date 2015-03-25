(in-package :cl-user)
(defpackage clack.middleware.session
  (:use :cl
        :clack.component
        :clack.middleware
        :clack.session.state
        :clack.session.state.cookie
        :clack.session.store)
  (:shadow :finalize :expire)
  (:import-from :alexandria :hash-table-plist))
(in-package :clack.middleware.session)
#.(rename-package :clack.session.state :clack.session.state '(state))

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-middleware-session> (<middleware>)
     ((state :type <clack-session-state>
             :initarg :state
             :initform
             (make-instance '<clack-session-state-cookie>)
             :accessor state)
      (store :type <clack-session-store>
             :initarg :store
             :initform
             (make-instance '<clack-session-store>)
             :accessor store))
  (:documentation "Clack Middleware for session management."))

(defmethod call ((this <clack-middleware-session>) env)
  (multiple-value-bind (id session) (extract this env)
    (setf (getf env :clack.session) (or session (make-hash-table :test 'equal)))
    (let ((options (make-hash-table :test #'equal)))
      (setf (gethash :id options) id)
      (setf (getf env :clack.session.options) options))

    (let ((res (call-next this env)))
      (finalize this env res))))

(defgeneric extract (mw env)
  (:documentation "Extract session id and state.")
  (:method ((this <clack-middleware-session>) env)
    (let* ((id (extract-id (state this) env))
           (session (when id
                      (fetch (store this) id))))
      (values (or id (generate-id (state this) env))
              session))))

(defgeneric finalize (mw env res)
  (:method ((this <clack-middleware-session>) env res)
    (let ((options (getf env :clack.session.options)))
      (unless (gethash :no-store options)
        (commit this env))
      (if (gethash :expire options)
          (expire this (gethash :id options) res env)
          (save-state this (gethash :id options) res env)))))

(defgeneric commit (mw env)
  (:method ((this <clack-middleware-session>) env)
    (let ((session (getf env :clack.session))
          (options (getf env :clack.session.options)))
      (cond
        ((gethash :expire options)
         (remove-session (store this) (gethash :id session)))
        ((gethash :change-id options)
         (remove-session (store this) (gethash :id session))
         (setf (gethash :id options) (generate-id (state this) env))
         (store-session (store this) (gethash :id options) session))
        (t
         (store-session (store this) (gethash :id options) session))))))

(defgeneric expire (mw id res env)
  (:method ((this <clack-middleware-session>) id res env)
    (state:expire
     (state this)
     id res
     (hash-table-plist (getf env :clack.session.options)))))

(defgeneric save-state (mw id res env)
  (:method ((this <clack-middleware-session>) id res env)
    (state:finalize
     (state this)
     id res
     (hash-table-plist (getf env :clack.session.options)))))
