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
- [API Reference](http://clacklisp.org/doc/#api)

## Server

* [Hunchentoot](http://weitz.de/hunchentoot/)
* Apache2 ([mod_lisp2](http://www.fractalconcept.com/asp/69t3/sdataQvWkQvUi-GrHDM==/asdataQuvY9x3g$ecX))
* [FastCGI](http://www.fastcgi.com/)

## Middleware

Middleware is one of the Clack Component. It takes another Application and runs it.

### Bundle Middleware

* [Clack.Middleware.Static](http://clacklisp.org/doc/clack.middleware.static.html) - Serves static files.
* [Clack.Middleware.Logger](http://clacklisp.org/doc/clack.middleware.logger.html) - Logging in Clack Application or Middleware.
* [Clack.Middleware.Session](http://clacklisp.org/doc/clack.middleware.session.html) - Session management.

### Contrib Middleware

* [Clack.Middleware.Dbi](http://clacklisp.org/doc/clack.middleware.dbi.html) - Middleware for CL-DBI connection management.
* [Clack.Middleware.Csrf](http://clacklisp.org/doc/clack.middleware.csrf.html) - Provides easy way to protect from CSRF.
* [Clack.Middleware.Auth.Basic](http://clacklisp.org/doc/clack.middleware.auth.basic.html) - Basic Authentication Middleware.
* [Clack.Middleware.Clsql](http://clacklisp.org/doc/clack.middleware.clsql.html) - For CLSQL connection management.
* [Clack.Middleware.Rucksack](http://clacklisp.org/doc/clack.middleware.rucksack.html) - For wrapping Rucksack transaction.
* [Clack.Middleware.OAuth](http://clacklisp.org/doc/clack.middleware.oauth.html) - Authorization by OAuth.

## Author

* Eitarow Fukamachi (e.arrows@gmail.com)

## Copyright

Copyright (c) 2011 Eitarow Fukamachi

## Contributors

* Tomohiro Matsuyama (tomo@cx4a.org)
* Norihisa Fujita (n.fujita@ariel-networks.com)

## License

Licensed under the LLGPL License.
