(in-package :cl-user)
(defpackage clack.test
  (:use :cl)
  (:import-from :clack
                :clackup
                :stop)
  (:import-from :prove
                :subtest)
  (:import-from :bordeaux-threads
                :thread-alive-p
                :destroy-thread)
  (:import-from :usocket
                :socket-listen
                :socket-close
                :address-in-use-error)
  (:export :*clack-test-handler*
           :*clack-test-port*
           :*enable-debug-p*
           :localhost
           :subtest-app))
(in-package :clack.test)

(defvar *clack-test-handler* :hunchentoot
  "Backend Handler to run tests on. String or Symbol are allowed.")

(defvar *clack-test-port* 4242
  "HTTP port number of Handler.")

(defvar *enable-debug-p* t)

(defun port-available-p (port)
  (let (socket)
    (unwind-protect
         (handler-case (setq socket (usocket:socket-listen "127.0.0.1" port :reuse-address t))
           (usocket:address-in-use-error () nil))
      (when socket
        (usocket:socket-close socket)
        T))))

(defun localhost (&optional (path "/") (port *clack-test-port*))
  (check-type path string)
  (setf path
        (cond
          ((= 0 (length path)) "/")
          ((not (char= (aref path 0) #\/))
           (concatenate 'string "/" path))
          (t path)))
  (format nil "http://localhost:~D~A"
          port path))

(defun %subtest-app (desc app client)
  (loop repeat 5
        until (port-available-p *clack-test-port*)
        do (sleep 0.1)
        finally
        (unless (port-available-p *clack-test-port*)
          (error "Port ~D is already in use." *clack-test-port*)))
  (let ((acceptor (clackup app
                           :server *clack-test-handler*
                           :use-thread t
                           :silent t
                           :port *clack-test-port*
                           :debug *enable-debug-p*)))
    (subtest desc
      (sleep 0.5)
      (unwind-protect
           (funcall client)
        (stop acceptor)))))

(defmacro subtest-app (desc app &body client)
  `(%subtest-app ,desc ,app (lambda () ,@client)))
