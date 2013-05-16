#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2013 Javier Olaechea <pirata@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(clack.util:namespace clack-middleware-json-preprocessor
  (:use :cl
        :clack)
  (:import-from :clack.util.stream
                :ensure-character-input-stream)
  (:import-from :alexandria :hash-table-plist)
  (:import-from :yason :parse)
  (:shadow :finalize :expire)
)

(cl-syntax:use-syntax :annot)

@export
(defclass <clack-middleware-json-preprocessor> (<middleware>)
  ())

(defmethod call ((this <clack-middleware-json-preprocessor>) env)
  (when (string-equal (getf env :content-type) "application/json")
    (setf (getf env :body-parameters)
          (list :json (yason:parse (ensure-character-input-stream
                                    (getf env :raw-body))))))
  (call-next this env))
