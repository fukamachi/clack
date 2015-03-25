(in-package :cl-user)
(defpackage t.clack.app.urlmap
  (:use :cl
        :clack.component
        :clack.app.urlmap
        :prove))
(in-package :t.clack.app.urlmap)

(plan 4)

(defparameter *urlmap* (make-instance '<clack-app-urlmap>))

(mount *urlmap* "/pc/" (lambda (env) (list 200 nil env)))
(mount *urlmap* "/api/" (lambda (env) (list 200 nil env)))

(is (call
     *urlmap*
     '(:path-info "/pc/hoge"
       :script-name ""
       :http-host "localhost:4242"
       :server-port 4242))
    '(200
      nil
      (:path-info "/hoge"
       :script-name "/pc/"
       :http-host "localhost:4242"
       :server-port 4242))
    "mount")

(is (call
     *urlmap*
     '(:path-info "/api/hoge"
       :script-name ""
       :http-host "localhost:4242"
       :server-port 4242))
    '(200
      nil
      (:path-info "/hoge"
       :script-name "/api/"
       :http-host "localhost:4242"
       :server-port 4242))
    "mount 2")

(is-expand (builder-urlmap
            ("/pc/" app-for-pc)
            ("/api/" app-for-api))
           (let (($urlmap (make-instance '<clack-app-urlmap>)))
             (mount $urlmap "/pc/" app-for-pc)
             (mount $urlmap "/api/" app-for-api)
             $urlmap)
           "builder-urlmap (expansion)")

(is-type (builder-urlmap
          ("/pc/" (lambda (env) (list 200 nil env)))
          ("/api/" (lambda (env) (list 200 nil env))))
         '<clack-app-urlmap>
         "builder-urlmap")

(finalize)
