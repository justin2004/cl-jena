# cl-jena

### What
A wrapper around some [Apache Jena](https://jena.apache.org/) functionality.

### Why
So you can play with [RDF triples](https://en.wikipedia.org/wiki/Semantic_triple) from Common Lisp.

### Dependencies
- [ABCL](https://abcl.org/)
- [Quicklisp](https://www.quicklisp.org) (although this project is not in Quicklisp -- it merely uses it locally)
- Apache Jena classes on your classpath (TODO document this)

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
```
