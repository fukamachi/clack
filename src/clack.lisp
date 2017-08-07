(in-package :cl-user)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :clack)
    (defpackage clack
      (:use :cl)
      (:import-from :clack.handler
                    :run
                    :stop)
      (:import-from :clack.util
                    :find-handler)
      (:import-from :lack
                    :builder)
      (:import-from :alexandria
                    :delete-from-plist)
      (:export :clackup
               :stop))))
(in-package :clack)

(defun eval-file (file)
  "Safer way to read and eval a file content. This function returns the last value."
  (setf file (probe-file file))
  (check-type file pathname)
  (with-open-file (in file)
    (let ((*package* *package*)
          (*readtable* *readtable*)
          (*load-pathname* file)
          (*load-truename* file))
      (loop with results
            with eof = '#:eof
            for form = (read in nil eof)
            until (eq form eof)
            do (setf results (multiple-value-list (eval form)))
            finally
               (return (apply #'values results))))))

(defmacro with-handle-interrupt (int-handler &body body)
  (let ((main (gensym "MAIN")))
    `(flet ((,main () ,@body))
       #+(or sbcl ccl clisp allegro ecl)
       (handler-case
           (let (#+ccl (ccl:*break-hook* (lambda (condition hook)
                                           (declare (ignore hook))
                                           (error condition))))
             (,main))
         (#+sbcl sb-sys:interactive-interrupt
          #+ccl  ccl:interrupt-signal-condition
          #+clisp system::simple-interrupt-condition
          #+ecl ext:interactive-interrupt
          #+allegro excl:interrupt-signal
          ()
           (funcall ,int-handler)))
       #-(or sbcl ccl clisp allegro ecl)
       (,main))))

(defun clackup (app &rest args
                &key (server :hunchentoot)
                  (port 5000)
                  (debug t)
                  silent
                  (use-thread #+thread-support t #-thread-support nil)
                  (use-default-middlewares t)
                  (interrupt-handler
                    (lambda () (format *error-output* "Interrupted")))
                &allow-other-keys)
  #-thread-support
  (when use-thread
    (error ":use-thread is T though there's no thread support."))
  (flet ((buildapp (app)
           (let ((app (typecase app
                        ((or pathname string)
                         (eval-file app))
                        (otherwise app))))
             (builder
              (if use-default-middlewares
                  :backtrace
                  nil)
              app))))
    (let ((app (buildapp app)))
      ;; Ensure the handler to be loaded.
      (find-handler server)
      (when (and (not use-thread)
                 (not silent))
        (format t "~&~:(~A~) server is going to start.~%Listening on localhost:~A.~%" server port))
      (with-handle-interrupt interrupt-handler
        (prog1
            (apply #'clack.handler:run app server
                   :port port
                   :debug debug
                   :use-thread use-thread
                   (delete-from-plist args :server :port :debug :silent :use-thread))
          (when (and use-thread
                     (not silent))
            (format t "~&~:(~A~) server is started.~%Listening on localhost:~A.~%" server port)))))))
