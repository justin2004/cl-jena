; vim: set filetype=lisp:

(eval-when (:compile-toplevel :load-toplevel :execute)
  (progn
    (require :abcl-contrib)
    (require :jss)
    (ql:quickload :cl-ppcre)
    (ql:quickload :cl-csv)
    (handler-case 
      (java:jstatic "init" (java:jclass "org.apache.jena.sys.JenaSystem"))
      (error (err) (progn 
                     (format t "is jena on your classpath?" 
                             (error "you need to put jena on your classpath~%e.g.~% (java:add-to-classpath #p\"/path/to/dependencies.jar\")")))))))


(defsystem "cl-jena"
           :description "A wrapper around some Apache Jena functionality"
           :serial t 
           ; :depends-on ((:require :abcl-contrib))
           ; :depends-on ((:require :jss))
           :depends-on (#:cl-ppcre #:cl-csv)
           :components ((:file "cl-jena")))

