#|
  This file is a part of Clack package.
  URL: http://github.com/fukamachi/clack
  Copyright (c) 2011 Eitaro Fukamachi <e.arrows@gmail.com>

  Clack is freely distributable under the LLGPL License.
|#

(in-package :cl-user)
(defpackage clack.handler
  (:use :cl)
  (:import-from :lack.util
                :find-package-or-load)
  (:import-from :bordeaux-threads
                :threadp
                :make-thread
                :thread-alive-p
                :destroy-thread)
  (:export :run
           :stop))
(in-package :clack.handler)

(defstruct handler
  server
  acceptor)

(defun find-handler (server)
  (flet ((find-with-prefix (prefix)
           (find-package-or-load (concatenate 'string
                                              prefix
                                              (symbol-name server)))))
    (or (find-with-prefix #.(string '#:clack.handler.))
        (error "~S is unknown handler."
               server))))

(defun run (app server &rest args &key use-thread &allow-other-keys)
  (let ((handler-package (find-handler server)))
    (flet ((run-server ()
             (apply (intern #.(string '#:run) handler-package)
                    app
                    :allow-other-keys t
                    args)))
      (make-handler
       :server server
       :acceptor (if use-thread
                     (bt:make-thread #'run-server
                                     :name (format nil "clack-handler-~(~A~)" server)
                                     :initial-bindings
                                     `((*standard-output* . ,*standard-output*)
                                       (*error-output* . ,*error-output*)))
                     (run-server))))))

(defun stop (handler)
  (let ((acceptor (handler-acceptor handler)))
    (if (bt:threadp acceptor)
        (progn
          (when (bt:thread-alive-p acceptor)
            (bt:destroy-thread acceptor))
          (sleep 0.5))
        (let ((package (find-handler (handler-server handler))))
          (funcall (intern #.(string '#:stop) package) acceptor)))
    t))
