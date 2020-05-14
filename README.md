# cl-jena

### What
Some wrappers around some [Apache Jena](https://jena.apache.org/) functionality.

### Why
So you can play with [RDF triples](https://en.wikipedia.org/wiki/Semantic_triple) from Common Lisp.

### Dependencies
- [ABCL](https://abcl.org/)
- [Quicklisp](https://www.quicklisp.org) (although this project is not in Quicklisp -- it merely uses it locally)
- Apache Jena classes on your classpath (TODO document this)

### Use
- put this directory in your quicklisp local-projects directory
    -  e.g. /home/user/quicklisp/local-projects/ 
- `(ql:quickload :cl-jena)` or `(asdf:load-system :cl-jena)`
