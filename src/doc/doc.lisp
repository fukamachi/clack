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
                :generate-documentation
                :find-system-packages)
  (:import-from :clack.doc.asdf
                :ensure-system-loaded))
(in-package :clack.doc)

(cl-annot:enable-annot-syntax)

@export
(defmethod generate-documentation ((system asdf:system))
  (ensure-system-loaded system)
  (let ((packages (find-system-packages system)))
    (apply
     #'concatenate
     'string
     (ignore-errors (slot-value system 'asdf::description))
     (mapcar #'generate-documentation
             (reverse packages)))))
