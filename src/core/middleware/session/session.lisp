#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.middleware.session
  (:use :cl
        :clack
        :clack.session.state
        :clack.session.state.cookie
        :clack.session.store)
  (:shadow :finalize :expire)
  (:import-from :alexandria :hash-table-plist))
#.(rename-package :clack.session.state :clack.session.state '(state))

(cl-annot:enable-annot-syntax)

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
    (setf (getf env :clack.session) session)
    (let ((options (make-hash-table :test #'equal)))
      (setf (gethash :id options) id)
      (setf (getf env :clack.session.options) options))

    (let ((res (call-next this env)))
      (finalize this env res))))

(defmethod extract ((this <clack-middleware-session>) env)
  "Extract session id and state."
  (let* ((id (extract-id (state this) env))
         (session (when id
                    (fetch (store this) id))))
    (values (or id (generate-id (state this) env))
            (or session (make-hash-table :test #'equal)))))

(defmethod finalize ((this <clack-middleware-session>) env res)
  (let ((options (getf env :clack.session.options)))
    (unless (gethash :no-store options)
      (commit this env))
    (if (gethash :expire options)
        (expire this (gethash :id options) res env)
        (save-state this (gethash :id options) res env))))

(defmethod commit ((this <clack-middleware-session>) env)
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
       (store-session (store this) (gethash :id options) session)))))

(defmethod expire ((this <clack-middleware-session>) id res env)
  (state:expire
   (state this)
   id res
   (hash-table-plist (getf env :clack.session.options))))

(defmethod save-state ((this <clack-middleware-session>) id res env)
  (state:finalize
   (state this)
   id res
   (hash-table-plist (getf env :clack.session.options))))

(doc:start)

@doc:NAME "
Clack.Middleware.Session - Middleware for session management.
"

@doc:SYNOPSIS "
    (clackup (builder
              (<clack-middleware-session>
               :state (make-instance '<clack-session-state-cookie>))
              (lambda (env)
                (sunless (gethash :counter (getf env :clack.session))
                  (setf it 0))
                `(200
                  (:content-type \"text/plain\")
                  (,(format nil \"Hello, you've been here for ~Ath times!\"
                            (incf (gethash :counter (getf env :clack.session)))))))))
"

@doc:DESCRIPTION "
Clack.Middleware.Session provides you session interface. By default this will use cookies to keep session state and store data in memory.

You can change this behavior to inherit `<clack-session-state>' and `<clack-session-store>'.

Note the `:clack.session' is a hash table, not a plist, because plist cannot keep state between functions.
"

@doc:AUTHOR "
Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Session.State
* Clack.Session.Store
"
