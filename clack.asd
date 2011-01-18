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
  (:use :asdf))

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
               :local-time)
  :components ((:module "src"
                :serial t
                :components
                ((:module "core"
                  :serial t
                  :components
                  ((:file "package")
                   (:file "request")
                   (:file "response")
                   (:module "handler"
                    :serial t
                    :components
                    ((:file "hunchentoot")
                     #+(or allegro cmu lispworks sbcl)
                     (:file "apache")))
                   (:file "component")
                   (:file "middleware")
                   (:file "main")))
                 (:module "contrib"
                  :depends-on ("core")
                  :serial t
                  :components
                  ((:module "app"
                    :components
                    ((:file "file")
                     (:file "route")))
                   (:module "middleware"
                    :components
                    ((:file "static")))))))))
