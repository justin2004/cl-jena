# cl-jena

### What
A wrapper around some [Apache Jena](https://jena.apache.org/) functionality.

### Why
So you can play with [RDF triples](https://en.wikipedia.org/wiki/Semantic_triple) using
[SPARQL](https://en.wikipedia.org/wiki/SPARQL) from Common Lisp.

### Dependencies
- [ABCL](https://abcl.org/)
- [Quicklisp](https://www.quicklisp.org) (although this project is not in Quicklisp -- it merely uses it locally)
- Apache Jena classes on your classpath
    - If you don't want to make your own jar you can download one I built:
        - `wget 'https://github.com/justin2004/spring-boot-rest-example/raw/master/shared-dependencies/target/shared-things-1.0.0-jar-with-dependencies.jar'`
        - `(java:add-to-classpath #p"/path/to/shared-things-1.0.0-jar-with-dependencies.jar")`

### How
- put this directory in your quicklisp local-projects directory
    -  e.g. /home/user/quicklisp/local-projects/ 
- `(ql:quickload :cl-jena)` or `(asdf:load-system :cl-jena)`

### Use

```
(in-package :jena)

; makes and sets a default model (*model*) and dataset (*ds*) for queries to target
(get-default-model)

(sparql-update "insert data { <http://bob.com/thing/0> <http://bob.com/has> 1 }")

(sparql-select "select * where {?s ?p ?o}")
; returns a list of alists (bindings) that satisfy the query
;   ((("p" . "http://bob.com/has") ("o" . 1) ("s" . "http://bob.com/thing/0")))

(write-dataset-to-file "/tmp/la.ttl")

; user@machine:/tmp$ cat /tmp/la.ttl 
; <http://bob.com/thing/0>
;         <http://bob.com/has>  1 .


; edit file, changing 1 to 100

(read-from-file-into-dataset "/tmp/la.ttl")

(sparql-select "select * where {?s ?p ?o}")
; the old triple and the new triple
;    ((("p" . "http://bob.com/has") ("o" . 1) ("s" . "http://bob.com/thing/0")) 
;     (("p" . "http://bob.com/has") ("o" . 100) ("s" . "http://bob.com/thing/0")))
```







```
;; example using owl inference

; start clean
(setf *ds* nil)

; make a new dataset with a default model
(jena::get-default-model)

; make an inference model (on top of the dataset *ds*)
(setf *infm*
      (jena::make-inf-model *ds*
                  (jena::make-owl-reasoner)))




; look for statements about henry in the dataset
(jena::print-alists
  (sparql-select "select * where {d2rq:henry ?p ?o}" 
                 :dataset *ds*))
; NIL

;; no statments yet



; look for statements about henry in the inference model
(jena::print-alists
  (sparql-select "select * where {d2rq:henry ?p ?o}" 
                 :dataset *ds*
                 :input-model *infm*))
; NIL

;; no statments yet




; add some statements to the dataset (which the inference model uses)
(sparql-update "insert data { 
                 d2rq:child a owl:class .
                 d2rq:person a owl:class .
                 d2rq:child rdfs:subClassOf d2rq:person .
                 d2rq:bob a d2rq:person .
                 d2rq:henry a d2rq:child .
                }" 
               :dataset *ds*
               :inf-model *infm*)

; look for statements about henry in the dataset again
(jena::print-alists
  (sparql-select "select * where {d2rq:henry ?p ?o}" 
                 :dataset *ds*))

; p: http://www.w3.org/1999/02/22-rdf-syntax-ns#type
; o: http://www.wiwiss.fu-berlin.de/suhl/bizer/D2RQ/0.1#child

;; notice that henry is a child like we said




; but since we know we can now infer that, since henry is child,
; he is also a person so let's look in the inference model

; look for statements about henry in the inference model again
(jena::print-alists
  (sparql-select "select * where {d2rq:henry ?p ?o}" 
                 :dataset *ds*
                 :input-model *infm*))

; p: http://www.w3.org/1999/02/22-rdf-syntax-ns#type
; o: http://www.wiwiss.fu-berlin.de/suhl/bizer/D2RQ/0.1#child
; 
; p: http://www.w3.org/1999/02/22-rdf-syntax-ns#type
; o: http://www.wiwiss.fu-berlin.de/suhl/bizer/D2RQ/0.1#person

;; and now you can see henry is also a person even though we didn't 
;; say so explicitly
```
