#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Testing Clack.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)

(defpackage clack-test-asd
  (:use :cl :asdf))

(in-package :clack-test-asd)

(defsystem clack-test
  :depends-on (:clack
               :cl-test-more
               :bordeaux-threads
               :drakma)
  :components
  ((:module "t"
    :components
    ((:module "core"
      :components
      ((:file "component")
       (:file "middleware")
       (:file "builder")
       (:file "response")
       (:file "request")
       (:file "handler/hunchentoot")
       (:file "app/file")
       (:file "middleware/static")
       (:file "middleware/session")
       (:file "middleware/logger")))
     (:module "contrib"
      :components
      ((:module "middleware"
        :components
        ((:file "csrf")))))
     (:module "util"
      :components
      ((:file "route")))))))
