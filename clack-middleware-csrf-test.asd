#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Testing Clack.Middleware.Csrf.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage clack-middleware-csrf-test-asd
  (:use :cl :asdf))
(in-package :clack-middleware-csrf-test-asd)

(defsystem clack-middleware-csrf-test
  :depends-on (:clack
               :clack-middleware-csrf
               :cl-test-more
               :drakma)
  :components
  ((:file "t/contrib/middleware/csrf"))
  :perform (load-op :after (op c) (asdf:clear-system c)))
