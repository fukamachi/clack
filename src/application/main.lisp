#|
  This file is a part of Slinky package.
  URL: http://github.com/fukamachi/slinky
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Slinky is freely distributable under the LLGPL License.
|#

#|
  Functions about Slinky Application.

  Author: Eitarow Fukamachi (fukamachi_e@ariel-networks.com)
|#

(in-package :slinky.application)

(defmacro defapp (name main-handler &key middleware root-dir)
  "Synonym of `(make-instance :name 'name ..)'."
  `(setf (symbol-value ',name)
         (make-instance '<slinky-application>
            :main ,main-handler
            :middleware ',middleware
            :root-dir ,root-dir)))
