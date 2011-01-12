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

(defmacro defapp (name routing &key
                  root-dir
                  (view-dir *default-view-dir*)
                  (action-dir *default-action-dir*)
                  (model-dir *default-model-dir*))
  "Define a Slinky application and regist it to `*applications*'."
  `(pushnew (make-instance '<application>
               :routing ',routing
               :name ,name
               :root-dir ,root-dir
               :view-dir ,view-dir
               :action-dir ,action-dir
               :model-dir ,model-dir)
            *applications*
            :key #'get-name))
