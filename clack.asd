#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

#|
  Clack is a Web server Interface for Common Lisp.

  Author: Eitaro Fukamachi (e.arrows@gmail.com)
|#

(in-package :cl-user)
(defpackage clack-asd
  (:use :cl :asdf))
(in-package :clack-asd)

(defsystem clack
  :version "1.1.0"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (;; Utility
               :trivial-types
               :alexandria
               :split-sequence
               :cl-syntax
               :cl-syntax-annot
               ;; for Other purpose
               :cl-ppcre
               :cl-fad
               :ironclad
               :http-body
               :quri
               :flexi-streams
               :local-time
               :circular-streams
               :cl-base64
               :marshal
               :trivial-mimes
               :trivial-backtrace
               :bordeaux-threads)
  :components ((:module "src"
                :components
                ((:module "core"
                  :depends-on ("util")
                  :components
                  ((:file "clack"
                    :depends-on ("component"
                                 "middleware"
                                 "handler"
                                 "file-watcher"))
                   (:file "builder"
                    :depends-on ("component" "middleware" "mw"))
                   (:file "request-response")
                   (:file "request" :depends-on ("request-response"))
                   (:file "response" :depends-on ("request-response"))
                   (:file "component")
                   (:file "middleware" :depends-on ("component"))
                   (:file "handler" :depends-on ("file-watcher"))
                   (:file "http-status")
                   (:module "app"
                    :depends-on ("clack")
                    :components
                    ((:file "file")
                     (:file "directory" :depends-on ("file"))
                     (:file "urlmap")))
                   (:file "file-watcher")
                   (:module "mw"
                    :pathname "middleware"
                    :depends-on ("clack" "component" "response" "request" "app")
                    :components
                    ((:file "static")
                     (:file "conditional")
                     (:module "session"
                      :serial t
                      :components
                      ((:file "state")
                       (:file "state/cookie")
                       (:file "store")
                       (:file "session")
                       (:file "cookie")))
                     (:file "accesslog")
                     (:file "let")
                     (:file "backtrace")))))
                 (:module "util"
                  :serial t
                  :components
                  ((:file "doc")
                   (:file "util"))))))
  :description "Web application environment for Common Lisp"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op t-clack)
                         (test-op t-clack-handler-hunchentoot)
                         (test-op t-clack-handler-wookie)
                         (test-op t-clack-handler-fcgi)
                         (test-op t-clack-handler-toot)
                         (test-op t-clack-middleware-csrf)
                         (test-op t-clack-middleware-auth-basic))))
