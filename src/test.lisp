(in-package :cl-user)
(defpackage clack.test
  (:use :cl)
  (:import-from :clack
                :clackup
                :stop)
  (:import-from :prove
                :subtest)
  (:import-from :usocket
                :socket-listen
                :socket-close
                :address-in-use-error
                :socket-error)
  (:export :*clack-test-handler*
           :*clack-test-port*
           :*clack-test-access-port*
           :*enable-debug*
           :*random-port*
           :localhost
           :subtest-app))
(in-package :clack.test)

(defvar *clack-test-handler* :hunchentoot
  "Backend Handler to run tests on. String or Symbol are allowed.")

(defvar *clack-test-port* 4242
  "HTTP port number of Handler.")

(defvar *clack-test-access-port* *clack-test-port*
  "Port of localhost to request.
Use if you want to set another port. The default is `*clack-test-port*`.")

(defvar *enable-debug* t)

(defvar *random-port* nil)

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

(defun random-port ()
  "Return a port number not in use from 50000 to 60000."
  (loop for port from (+ 50000 (random 1000)) upto 60000
        if (port-available-p port)
          return port))

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
  (let* ((*clack-test-port* (if *random-port*
                                (random-port)
                                *clack-test-port*))
         (*clack-test-access-port* (if *random-port*
                                       *clack-test-port*
                                       *clack-test-access-port*)))
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
                             :debug *enable-debug*)))
      (subtest desc
        (sleep 0.5)
        (unwind-protect
             (funcall client)
          (stop acceptor))))))

(defmacro subtest-app (desc app &body client)
  `(%subtest-app ,desc ,app (lambda () ,@client)))
