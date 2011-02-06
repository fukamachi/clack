#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitarow Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack is a Web server Interface for Common Lisp.

  Author: Eitarow Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)

(defpackage clack-asd
  (:use :cl :asdf))

(in-package :clack-asd)

(defsystem clack
  :version "1.0"
  :author "Eitarow Fukamachi"
  :license "LLGPL"
  :depends-on (;; Utility
               :alexandria
               :metabang-bind
               :anaphora
               :split-sequence
               ;; Server
               :hunchentoot
               #+(or allegro cmu lispworks sbcl)
               :modlisp
               ;; for Other purpose
               :cl-ppcre
               :cl-fad
               :cl-test-more
               :ironclad
               :drakma)
  :components ((:module "src"
                :serial t
                :components
                ((:module "util"
                  :components
                  ((:file "util")
                   (:file "hunchentoot")))
                 (:module "core"
                  :components
                  ((:file "component")
                   (:file "middleware" :depends-on ("component"))
                   (:file "builder" :depends-on ("middleware"))
                   (:file "response")
                   (:file "request")
                   (:module "handler"
                    :depends-on ("component")
                    :components
                    ((:file "hunchentoot")
                     #+(or allegro cmu lispworks sbcl)
                     (:file "apache")))
                   (:file "test")
                   (:file "test/suite" :depends-on ("test"))))
                 (:module "contrib"
                  :depends-on ("core")
                  :serial t
                  :components
                  ((:module "core/session"
                    :serial t
                    :components
                    ((:file "state")
                     (:file "state/cookie")
                     (:file "store")))
                   (:module "app"
                    :components
                    ((:file "file")
                     (:file "route")))
                   (:module "middleware"
                    :components
                    ((:file "static")
                     (:file "session")))))))))

;; Run unit tests.
#|
(defmethod asdf:perform :after ((op load-op) (c (eql (find-system :clack))))
  (asdf:oos 'asdf:load-op :clack-test))
|#
