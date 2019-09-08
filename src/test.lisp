(in-package :cl-user)
(defpackage clack.test
  (:use :cl)
  (:import-from :clack
                :clackup
                :stop)
  (:import-from :dexador
                :*use-connection-pool*)
  (:import-from :rove
                :testing)
  (:import-from :usocket
                :socket-listen
                :socket-close
                :address-in-use-error
                :socket-error)
  (:export :*clack-test-handler*
           :*clack-test-port*
           :*clack-test-access-port*
           :*clackup-additional-args*
           :*enable-debug*
           :*random-port*
           :localhost
           :testing-app))
(in-package :clack.test)

(defvar *clack-test-handler* :hunchentoot
  "Backend Handler to run tests on. String or Symbol are allowed.")

(defvar *clack-test-port* 4242
  "HTTP port number of Handler.")

(defvar *clackup-additional-args* '()
  "Additional arguments for clackup.")

(defvar *clack-test-access-port* *clack-test-port*
  "Port of localhost to request.
Use if you want to set another port. The default is `*clack-test-port*`.")

(defvar *enable-debug* t)

(defvar *random-port* t)

(defun port-available-p (port)
  (let (socket)
    (unwind-protect
         (handler-case (progn
                         (setq socket (usocket:socket-listen "127.0.0.1" port :reuse-address t))
                         t)
           (usocket:address-in-use-error () nil)
           (usocket:socket-error (e)
             (warn "USOCKET:SOCKET-ERROR: ~A" e)
             nil))
      (when socket
        (usocket:socket-close socket)
        t))))

(defun server-running-p (port)
  (handler-case (let ((socket (usocket:socket-connect "127.0.0.1" port)))
                  (usocket:socket-close socket)
                  t)
    (usocket:connection-refused-error () nil)
    (usocket:connection-reset-error () nil)))

(defun random-port ()
  "Return a port number not in use from 50000 to 60000."
  (loop for port from (+ 50000 (random 1000)) upto 60000
        if (port-available-p port)
          return port))

(defun localhost (&optional (path "/") (port *clack-test-access-port*))
  (check-type path string)
  (setf path
        (cond
          ((= 0 (length path)) "/")
          ((not (char= (aref path 0) #\/))
           (concatenate 'string "/" path))
          (t path)))
  (format nil "http://127.0.0.1:~D~A"
          port path))

(defun %testing-app (app client)
  (let* ((*clack-test-port* (if *random-port*
                                (random-port)
                                *clack-test-port*))
         (*clack-test-access-port* (if *random-port*
                                       *clack-test-port*
                                       *clack-test-access-port*))
         (threads #+thread-support (bt:all-threads)
                  #-thread-support '()))
    (loop repeat 5
          until (port-available-p *clack-test-port*)
          do (sleep 0.1)
          finally
             (unless (port-available-p *clack-test-port*)
               (error "Port ~D is already in use." *clack-test-port*)))
    (let ((acceptor (apply #'clackup app
                           :server *clack-test-handler*
                           :port *clack-test-port*
                           :debug *enable-debug*
                           :use-thread t
                           :silent t
                           *clackup-additional-args*))
          (dex:*use-connection-pool* nil))
      (loop until (server-running-p *clack-test-port*)
            do (sleep 0.1))

      (multiple-value-prog1
          (unwind-protect (funcall client)
            (stop acceptor)
            ;; Ensure all threads are finished for preventing from leaking
            #+thread-support
            (dolist (thread (bt:all-threads))
              (when (and (not (find thread threads))
                         (bt:thread-alive-p thread))
                (bt:destroy-thread thread))))

        (loop while (server-running-p *clack-test-port*)
              do (sleep 0.1))))))

(defmacro testing-app (desc app &body body)
  `(%testing-app ,app (lambda () (testing ,desc ,@body))))
