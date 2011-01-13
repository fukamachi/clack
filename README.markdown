# Clack - Web server Interface for Common Lisp

Clack is a Web server Interface for Common Lisp inspired by Python's WSGI and Ruby's Rack.

Now works on Hunchentoot only.

## Usage

    (defpackage simple-app
      (:use :cl :clack))
    
    (in-package :simple-app)
    
    (defvar app
      (lambda (env)
        '(200 (:content-type "text/plain") ("Hello, Clack!"))))
    
    (run app)

Now access [http://localhost:8080/](http://localhost:8080/) and Clack show you "Hello, Clack!".

## Middleware

    (defpackage simple-app
      (:use :cl :clack))
    
    (in-package :simple-app)
    
    (defclass <simple-middleware> (<middleware>) ())
    (defmethod call ((mw <simple-middleware>) env)
      '(200 (:content-type "text/plain") ("Hello, Clack Middleware!")))
    (defmethod build ((mw <middleware>) app)
      (lambda (env)
        (let ((res (funcall app env)))
          (reverse (cons (call mw env) (cdr (reverse res)))))))
    
    (defvar mw (make-instance '<simple-middleware>))
    
    (run (build mw app))

## Dependency

* Hunchentoot

## License

Copyright (c) 2011 Eitarow Fukamachi  
Licensed under the LLGPL License.
