#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack.session.store
  (:use :cl)
  (:import-from :alexandria
                :remove-from-plistf)
  (:import-from :clack.util :getf*))

(cl-annot:enable-annot-syntax)

@export
(defclass <clack-session-store> ()
     ((stash :type list
             :initform nil
             :accessor stash)))

@export
(defmethod fetch ((this <clack-session-store>) sid)
  (getf* (stash this) sid))

@export
(defmethod store-session ((this <clack-session-store>) sid session)
  (setf (getf* (stash this) (intern sid :keyword)) session))

@export
(defmethod remove-session ((this <clack-session-store>) sid)
  (remove-from-plistf (stash this) sid))

(doc:start)

@doc:NAME "
Clack.Session.Store - Basic in-memory session store.
"

@doc:DESCRIPTION "
Clack.Session.Store is basic in-memory session data store. This is volatile storage and not recommended for multiprocessing environments. However this is very useful for development and testing.
"

@doc:AUTHOR "
Eitarow Fukamachi (e.arrows@gmail.com)
"

@doc:SEE "
* Clack.Middleware.Session
"
