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
(defpackage t-clack-asd
  (:use :cl :asdf))
(in-package :t-clack-asd)

(defsystem t-clack
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
       (:file "app/urlmap")
       (:file "middleware/static")
       (:file "middleware/conditional")
       (:file "middleware/session")
       (:file "middleware/logger")))
     (:module "util"
      :components
      ((:file "route"))))))
  :perform (load-op :after (op c) (asdf:clear-system c)))
