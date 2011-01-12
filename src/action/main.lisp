#|
  This file is a part of Slinky package.
  URL: http://github.com/fukamachi/slinky
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Slinky is freely distributable under the LLGPL License.
|#

#|
  Functions about Slinky action.

  Author: Eitarow Fukamachi (fukamachi_e@ariel-networks.com)
|#

(in-package :slinky.action)

(defmacro defaction (name query-params-list &body body)
  "Define a Slinky action and regist it to `*actions*'."
  `(pushnew (make-instance '<action>
               :name ,name
               :params-list ,query-params-list
               :body (lambda (,@query-params-list)
                       (let ((*action* ,name))
                         ,@body)))
            *actions*
            :key #'get-name))
