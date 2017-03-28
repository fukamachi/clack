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
  :version "2.0.0"
  :author "Eitaro Fukamachi"
  :license "LLGPL"
  :depends-on (:lack
               :lack-middleware-backtrace
               :lack-util
               :bordeaux-threads
               :alexandria
               :uiop)
  :components ((:module "src"
                :components
                ((:file "clack" :depends-on ("handler" "util"))
                 (:file "handler" :depends-on ("util"))
                 (:file "util"))))
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
          seq))))
