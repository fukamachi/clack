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
  (:import-from :usocket)
  (:export :run
           :stop))
(in-package :clack.handler)

(defun server-running-p (port)
  (handler-case (let ((socket (usocket:socket-connect "127.0.0.1" port)))
                  (usocket:socket-close socket)
                  t)
    (usocket:socket-error () nil)))

(defstruct handler
  server
  port
  acceptor)

(defun run (app server &rest args &key port use-thread &allow-other-keys)
  (let ((handler-package (find-handler server))
        (bt:*default-special-bindings* `((*standard-output* . ,*standard-output*)
                                         (*error-output* . ,*error-output*)
                                         ,@bt:*default-special-bindings*)))
    (flet ((run-server ()
             (apply (intern #.(string '#:run) handler-package)
                    app
                    :allow-other-keys t
                    args)))
      (prog1
          (make-handler
           :server server
           :port port
           :acceptor (if use-thread
                         (bt:make-thread #'run-server
                                         :name (format nil "clack-handler-~(~A~)" server)
                                         :initial-bindings
                                         `((bt:*default-special-bindings* . ',bt:*default-special-bindings*)
                                           ,@bt:*default-special-bindings*))
                         (run-server)))
        (loop until (server-running-p port)
              do (sleep 0.1))))))

(defun stop (handler)
  (let ((acceptor (handler-acceptor handler)))
    (if (bt:threadp acceptor)
        (when (bt:thread-alive-p acceptor)
          (bt:destroy-thread acceptor)
          (loop while (bt:thread-alive-p acceptor)
                do (sleep 0.1)))
        (let ((package (find-handler (handler-server handler))))
          (funcall (intern #.(string '#:stop) package) acceptor)
          (loop while (server-running-p (handler-port handler))
                do (sleep 0.1))))
    t))
