#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.session.store
  (:use :cl))
(in-package :clack.session.store)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-session-store> ()
     ((stash :type hash-table
             :initform (make-hash-table :test 'equal)
             :accessor stash)))

@export
(defgeneric fetch (store sid)
  (:method ((this <clack-session-store>) sid)
    (gethash sid (stash this))))

@export
(defgeneric store-session (store sid session)
  (:method ((this <clack-session-store>) sid session)
    (setf (gethash sid (stash this))
          session)))

@export
(defgeneric remove-session (store sid)
  (:method ((this <clack-session-store>) sid)
    (remhash sid (stash this))))

(doc:start)

@doc:NAME "
Clack.Session.Store - Basic in-memory session store.
"

@doc:DESCRIPTION "
Clack.Session.Store is a basic in-memory session data store. This is volatile storage and not recommended for multiprocessing environments. However, this is very useful for development and testing.
"

@doc:AUTHOR "
Eitaro Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Middleware.Session
"
