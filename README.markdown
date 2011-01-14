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
      (:use :cl :clack)
      (:export :<simple-middleware>))
    
    (in-package :clack.middleware.example)
    
    (defclass <simple-middleware> (<middleware>) ())
    (defmethod call ((self <simple-middleware>) req)
      `(200 (:content-type "text/html")
        ,(cons "Hello, Clack Middleware!<br />"
               (nth 2 (funcall (app self) req)))))

Use.

    (defpackage simple-app
      (:use :cl :clack :clack.middleware.example))
    
    (in-package :simple-app)
    
    (defvar app
      (lambda (req)
        '(200 (:content-type "text/plain") ("Hello, Clack!"))))
    
    (run (builder <simple-middleware> app))

And you should get following response in time.

    Hello, Clack Middleware!
    Hello, Clack!

## Dependency

* Hunchentoot

## License

Copyright (c) 2011 Eitarow Fukamachi  
Licensed under the LLGPL License.
