(in-package :cl-user)
(defpackage :clack-middleware-json-preprocessor-asd
  (:use :cl :asdf))
(in-package :clack-middleware-json-preprocessor-asd)

(defsystem clack-middleware-json-preprocessor
  :version "0.1"
  :author "Javier Olaechea"
  :license "GPLv3"
  :depends-on (:clack
               :cl-syntax
               :cl-syntax-annot
               :alexandria
               :yason)
  :components ((:file "src/contrib/middleware/json-preprocessor"))
  :description "Middleware that adds a :json the environment if the request has a content-type application/json")
