(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :clack)
    (defpackage clack
      (:use :cl)
      (:import-from :clack.handler
                    :run
                    :stop)
      (:import-from :lack
                    :builder)
      (:import-from :alexandria
                    :delete-from-plist)
      (:export :clackup
               :stop))))
(in-package :clack)

(defun eval-file (file)
  "Safer way to read and eval a file content. This function returns the last value."
  (check-type file (or pathname string))
  (with-open-file (in file)
    (let ((*package* *package*)
          (*readtable* *readtable*)
          (*load-pathname* nil)
          (*load-truename* nil))
      (loop with results
            with eof = '#:eof
            for form = (read in nil eof)
            until (eq form eof)
            do (setf results (multiple-value-list (eval form)))
            finally
               (return (apply #'values results))))))

(defun clackup (app &rest args
                &key (server :hunchentoot)
                  (port 5000)
                  (debug t)
                  silent
                  (use-thread #+thread-support t #-thread-support nil)
                  (use-default-middlewares t)
                &allow-other-keys)
  (flet ((print-start-message ()
           (unless silent
             (format t "~&~:(~A~) server is started.~%Listening on localhost:~A.~%" server port)))
         (buildapp (app)
           (let ((app (typecase app
                        ((or pathname string)
                         (eval-file app))
                        (otherwise app))))
             (builder
              (if use-default-middlewares
                  :backtrace
                  nil)
              app))))
    (unless use-thread
      (print-start-message))
    (prog1
        (apply #'clack.handler:run (buildapp app) server
               :port port
               :debug debug
               :use-thread use-thread
               (delete-from-plist args :server :port :debug :silent :use-thread))
      (print-start-message))))
