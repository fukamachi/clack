# Clack - Web Application Environment for Common Lisp

Clack is a web application environment for Common Lisp inspired by Python's WSGI and Ruby's Rack. Your awesome framework should base on this.

## Usage

    (defpackage simple-app
      (:use :cl
            :clack))
    (in-package :simple-app)
    
    (defvar *handler*
        (clackup
          #'(lambda (env)
              '(200 (:content-type "text/plain") ("Hello, Clack!")))))

Open your web browser and go to [http://localhost:5000/](http://localhost:5000/). You should get "Hello, Clack!".

To stop the server, use `(clack:stop *handler*)`.

## Installation

Clack is available on [Quicklisp](http://www.quicklisp.org/beta/).

    (ql:quickload :clack)

## Documentation

- [Tutorial](http://clacklisp.org/tutorial/)
- [Quickdocs Page](http://quickdocs.org/clack/)

## Server

* [Hunchentoot](http://weitz.de/hunchentoot/)
* Apache2 ([mod_lisp2](http://www.fractalconcept.com/asp/69t3/sdataQvWkQvUi-GrHDM==/asdataQuvY9x3g$ecX))
* [FastCGI](http://www.fastcgi.com/)
* [Toot](https://github.com/gigamonkey/toot)

## Middleware

Middleware is a type of Clack components, which modifies the behavior of a Clack application without modifing the Clack application's code.

### Bundle Middleware

* [Clack.Middleware.Static](http://quickdocs.org/clack/api#package-CLACK.MIDDLEWARE.STATIC) - returns static files
* [Clack.Middleware.Logger](http://quickdocs.org/clack/api#package-CLACK.MIDDLEWARE.LOGGER) - logs messages
* [Clack.Middleware.Session](http://quickdocs.org/clack/api#package-CLACK.MIDDLEWARE.SESSION) - manages sessions
* [Clack.Middleware.Conditional](http://quickdocs.org/clack/api#package-CLACK.MIDDLEWARE.CONDITIONAL) - enables a middleware to conditionally wraps components
* [Clack.Middleware.Auth.Basic](http://quickdocs.org/clack/api#package-CLACK.MIDDLEWARE.AUTH.BASIC) - provides basic authentication


### Contrib Middleware

* [Clack.Middleware.Dbi](http://quickdocs.org/clack/api#system-clack-middleware-dbi) - manages CL-DBI connections
* [Clack.Middleware.Csrf](http://quickdocs.org/clack/api#system-clack-middleware-csrf) - provides an easy way to protect an application from CSRF attacks
* [Clack.Middleware.Clsql](http://quickdocs.org/clack/api#system-clack-middleware-clsql) - manages CLSQL connections
* [Clack.Middleware.Postmodern](http://quickdocs.org/clack/api#system-clack-middleware-postmodern) - manages POSTMODERN connections
* [Clack.Middleware.Rucksack](http://quickdocs.org/clack/api#system-clack-middleware-rucksack) - wraps Rucksack transaction.
* [Clack.Middleware.OAuth](http://quickdocs.org/clack/api#system-clack-middleware-oauth) - provides OAuth authorization

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011-2014 Eitarow Fukamachi

## Contributors

* https://github.com/fukamachi/clack/graphs/contributors

## License

Licensed under the LLGPL License.
