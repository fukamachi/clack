# Slinky - Web Application Environment for Common Lisp

Slinky is a Web Application Environment for Common Lisp inspired by Rack and WSGI.

Now works on Hunchentoot only.

## Usage

    (defpackage simple-blog
      (:use :cl :slinky))
    
    (in-package :simple-blog)
    
    (defapp blog
      (lambda (req) "Hello, Slinky!"))
    
    (run blog)

Now access [http://localhost:8080/](http://localhost:8080/) and Slinky show you "Hello, Slinky!".

## Dependency

* Hunchentoot
* Closer MOP
* Alexandria

## License

Copyright (c) 2011 Eitarow Fukamachi  
Licensed under the LLGPL License.
