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
  `(make-instance '<slinky-application>
      :name ',name
      :main ,main-handler
      :middleware ',middleware
      :root-dir ,root-dir))

(defun find-app (name &key (test #'eq))
  "Find an application with the given name and return the instance."
  (find name
        (instance-collection (find-class '<slinky-application>))
        :test test
        :key #'get-name))
