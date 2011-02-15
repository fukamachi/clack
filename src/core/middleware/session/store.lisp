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
        :alexandria))

(cl-annot:enable-annot-syntax)

@export
(defclass <clack-session-store> ()
     ((stash :initform nil :accessor stash)))

@export
(defmethod fetch ((this <clack-session-store>) sid)
  (getf* (stash this) sid))

@export
(defmethod store-session ((this <clack-session-store>) sid session)
  (setf (getf* (stash this) (intern sid :keyword)) session))

@export
(defmethod remove-session ((this <clack-session-store>) sid)
  (remove-from-plistf (stash this) sid))
