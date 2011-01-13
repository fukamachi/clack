# Clack - Web server Interface for Common Lisp

Clack is a Web server Interface for Common Lisp inspired by Python's WSGI and Ruby's Rack.

Now works on Hunchentoot only.

## Usage

    (defpackage simple-app
      (:use :cl :clack))
    
    (in-package :simple-app)
    
    (defvar app
      (lambda (env) "Hello, Clack!"))
    
    (run app)

Now access [http://localhost:8080/](http://localhost:8080/) and Clack show you "Hello, Clack!".

## Dependency

* Hunchentoot

## License

Copyright (c) 2011 Eitarow Fukamachi  
Licensed under the LLGPL License.
