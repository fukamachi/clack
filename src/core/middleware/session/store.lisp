#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.session.store
  (:use :cl)
  (:import-from :alexandria
                :make-keyword
                :remove-from-plistf)
  (:import-from :clack.util :getf*))
(in-package :clack.session.store)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-session-store> ()
     ((stash :type list
             :initform nil
             :accessor stash)))

@export
(defgeneric fetch (store sid)
  (:method ((this <clack-session-store>) sid)
    (getf* (stash this) sid)))

@export
(defgeneric store-session (store sid session)
  (:method ((this <clack-session-store>) sid session)
    (setf (getf* (stash this) (make-keyword sid))
          session)))

@export
(defgeneric remove-session (store sid)
  (:method ((this <clack-session-store>) sid)
    (remove-from-plistf (stash this) sid)))

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
