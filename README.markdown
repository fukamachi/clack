# Clack - Web Application Environment for Common Lisp

Clack is a Web Application Environment for Common Lisp inspired by Python's WSGI and Ruby's Rack. Your awesome framework should base on this.

## Usage

    (defpackage simple-app
      (:use :cl :clack :clack.handler.hunchentoot))
    
    (in-package :simple-app)
    
    (defvar app
      (lambda (req)
        '(200 (:content-type "text/plain") ("Hello, Clack!"))))
    
    (run app)

Now access [http://localhost:8080/](http://localhost:8080/) and Clack show you "Hello, Clack!".

## Handler

* clack.handler.hunchentoot
* clack.handler.apache

## Application

## Middleware

### Use Middleware

#### clack.middleware.static

    (defpackage simple-app
      (:use :cl
            :clack
            :clack.handler.hunchentoot
            :clack.middleware.static))
    
    (in-package :simple-app)
    
    (defvar app
      (lambda (req)
        '(200 (:content-type "text/plain") ("Hello, Clack!"))))
    
    (run
     (builder
      (<clack-middleware-static>
       :urls '(#p"favicon.ico" #p"404.html")
       :root #p"/public/")
      app))

### How to write Middleware?

    (defpackage clack.middleware.example
      (:use :cl :clack)
      (:export :<simple-middleware>))
    
    (in-package :clack.middleware.example)
    
    (defclass <simple-middleware> (<middleware>) ())
    (defmethod call ((this <simple-middleware>) req)
      `(200 (:content-type "text/html")
        ,(cons "Hello, Clack Middleware!<br />"
               (nth 2 (call-next this req)))))

    (defpackage simple-app
      (:use :cl
            :clack
            :clack.handler.hunchentoot
            :clack.middleware.example))
    
    (in-package :simple-app)
    
    (defvar app
      (lambda (req)
        '(200 (:content-type "text/plain") ("Hello, Clack!"))))
    
    (run (builder <simple-middleware> app))

And you should get following response in time.

    Hello, Clack Middleware!
    Hello, Clack!

## Handler

## Request

* :request-method
* :script-name
* :path-info
* :query-string
* :server-name
* :server-port
* :request-uri
* :server-protocol
* :http-server
* :%request
* :http-*

## Dependency

* CL-PPCRE
* CL-FAD
* local-time
* Alexandria
* SPLIT-SEQUENCE
* Anaphora
* metabang-bind
* Hunchentoot
* modlisp

## License

Copyright (c) 2011 Eitarow Fukamachi  
Licensed under the LLGPL License.
