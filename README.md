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
