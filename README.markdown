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

Now access [http://localhost:5000/](http://localhost:5000/) and Clack should show you "Hello, Clack!".

To stop the server, execute `(clack:stop *handler*)`.

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

## Middleware

Middleware is one of the Clack Component. It takes another Application and runs it.

### Bundle Middleware

* [Clack.Middleware.Static](http://quickdocs.org/clack/api#package-CLACK.MIDDLEWARE.STATIC) - Serves static files.
* [Clack.Middleware.Logger](http://quickdocs.org/clack/api#package-CLACK.MIDDLEWARE.LOGGER) - Logging in Clack Application or Middleware.
* [Clack.Middleware.Session](http://quickdocs.org/clack/api#package-CLACK.MIDDLEWARE.SESSION) - Session management.

### Contrib Middleware

* [Clack.Middleware.Dbi](http://quickdocs.org/clack/api#system-clack-middleware-dbi) - Middleware for CL-DBI connection management.
* [Clack.Middleware.Csrf](http://quickdocs.org/clack/api#system-clack-middleware-csrf) - Provides easy way to protect from CSRF.
* [Clack.Middleware.Auth.Basic](http://quickdocs.org/clack/api#system-clack-middleware-auth-basic) - Basic Authentication Middleware.
* [Clack.Middleware.Clsql](http://quickdocs.org/clack/api#system-clack-middleware-clsql) - For CLSQL connection management.
* [Clack.Middleware.Postmodern](http://quickdocs.org/clack/api#system-clack-middleware-postmodern) - Middleware for POSTMODERN connection management.
* [Clack.Middleware.Rucksack](http://quickdocs.org/clack/api#system-clack-middleware-rucksack) - For wrapping Rucksack transaction.
* [Clack.Middleware.OAuth](http://quickdocs.org/clack/api#system-clack-middleware-oauth) - Authorization by OAuth.

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011 Eitarow Fukamachi

## Contributors

* https://github.com/fukamachi/clack/graphs/contributors

## License

Licensed under the LLGPL License.
