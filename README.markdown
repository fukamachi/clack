# Clack - Web server Interface for Common Lisp

Clack is a Web server Interface for Common Lisp inspired by Python's WSGI and Ruby's Rack.

Now works on Hunchentoot only.

## Usage

    (defpackage simple-app
      (:use :cl :clack))
    
    (in-package :simple-app)
    
    (defvar app
      (lambda (req)
        '(200 (:content-type "text/plain") ("Hello, Clack!"))))
    
    (run app)

Now access [http://localhost:8080/](http://localhost:8080/) and Clack show you "Hello, Clack!".

## Middleware

Write.

    (defpackage clack.middleware.example
      (:use :cl :clack))
    
    (in-package :clack.middleware.example)
    
    (defclass <simple-middleware> (<middleware>) ())
    (defmethod call ((mw <simple-middleware>) req)
      '(200 (:content-type "text/plain") ("Hello, Clack Middleware!")))
    (defmethod wrap ((mw <middleware>) app &rest args)
      (lambda (req)
        (let ((res (funcall app req)))
          (reverse (cons (call mw req) (cdr (reverse res)))))))

Use.

    (defpackage simple-app
      (:use :cl :clack :clack.middleware.example))
    
    (in-package :simple-app)
    
    (defvar app
      (lambda (req)
        '(200 (:content-type "text/plain") ("Hello, Clack!"))))
    
    (run (builder '<simple-middleware> app))

## Dependency

* Hunchentoot

## License

Copyright (c) 2011 Eitarow Fukamachi  
Licensed under the LLGPL License.
