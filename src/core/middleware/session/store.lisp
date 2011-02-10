#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack.Session.Store
  Basic in-memory session store

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(clack.util:namespace clack.session.store
  (:use :cl
        :clack.util
        :alexandria)
  (:export :<clack-session-store>
           :fetch
           :store-session
           :remove-session))

(defclass <clack-session-store> ()
     ((stash :initform nil :accessor stash)))

(defmethod fetch ((this <clack-session-store>) sid)
  (getf* (stash this) sid))

(defmethod store-session ((this <clack-session-store>) sid session)
  (setf (getf* (stash this) (intern sid :keyword)) session))

(defmethod remove-session ((this <clack-session-store>) sid)
  (remove-from-plistf (stash this) sid))
