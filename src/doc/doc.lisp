#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.doc
  (:use :cl)
  (:import-from :clack.doc.class
                :generate-documentation)
  (:import-from :clack.doc.asdf
                :asdf-system-packages))
(in-package :clack.doc)

(cl-annot:enable-annot-syntax)

@export
(defmethod generate-documentation ((system asdf:system))
  (let ((packages (asdf-system-packages system)))
    (apply
     #'concatenate
     'string
     (ignore-errors (slot-value system 'asdf::description))
     (loop for pkg in (reverse packages)
           append (mapcar #'generate-documentation
                            (gethash pkg clack.doc.class::*package-symbols-hash*))))))
