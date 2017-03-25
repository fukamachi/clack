(in-package :cl-user)
(defpackage clack.handler
  (:use :cl)
  (:import-from :clack.util
                :find-handler)
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

(defun run (app server &rest args &key use-thread &allow-other-keys)
  (let ((handler-package (find-handler server))
        (bt:*default-special-bindings* `((*standard-output* . ,*standard-output*)
                                         (*error-output* . ,*error-output*)
                                         ,@bt:*default-special-bindings*)))
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
                                     `((bt:*default-special-bindings* . ',bt:*default-special-bindings*)
                                       ,@bt:*default-special-bindings*))
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
