(defpackage :cl-jena
  (:nicknames :jena)
  (:use :common-lisp :java :jss)
  (:export
    #:get-query-execution
    #:get-update-processor
    #:load-triples
    #:model->jsonld-embedded
    #:print-result-set
    #:sparql-construct
    #:sparql-select
    #:sparql-update
    #:*prefix-list*
    #:*ds*
    #:*model*
    #:get-default-model
    #:write-dataset-to-file 
    #:read-from-file-into-dataset 
    ))

(in-package :cl-jena)

; the reader needs :jss for the (#"method" object) reader macros
(require :abcl-contrib)
(require :jss)

(require :cl-ppcre)

(defvar *prefix-list*) 

; defaults for playing around
(defvar *ds* nil) 
(defvar *model* nil) 

(setf *prefix-list*
      (list "prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>"
            "prefix d2rq: <http://www.wiwiss.fu-berlin.de/suhl/bizer/D2RQ/0.1#>"
            "prefix xsd: <http://www.w3.org/2001/XMLSchema#>"
            "prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#>"
            "prefix owl: <http://www.w3.org/2002/07/owl#>"))

; to toString method for a ModelCom will print all the triples in the model.
; if you have a model with lots of triples this could use alot of memory.
(defmethod print-object ((obj (java:jclass "org.apache.jena.rdf.model.impl.ModelCom")) stream)
  (format t "a ModelCom~%"))


; creates a memory backed tdb2 dataset which works for testing
(defun get-default-model ()
  "sets *model* and *ds* to default model and the corresponding dataset, respectively"
  (if (null *ds*)
      ; start from scratch
      (progn
        (setf *ds*
              (jstatic "createDataset" (jclass "org.apache.jena.tdb2.TDB2Factory")))
        (setf *model* (#"getDefaultModel" *ds*)))
      ; use existing dataset
      (progn
        (setf *model* (#"getDefaultModel" *ds*))
        t)))

; TODO assumes turtle files
(defun load-triples (path &optional (dataset *ds*))
  "give a path (string) to a directory and a dataset load all the .ttl files from the path into the dataset"
  (let ((turtle-files (remove nil (mapcar #'(lambda (x)
                                  (if (ppcre:scan "\.ttl$" x)
                                      x
                                      nil))
                              (mapcar #'namestring
                                      (uiop:directory-files path)))))
        (model (#"getDefaultModel" dataset)))
    (mapcar #'(lambda (file)
                (progn
                  (format t "loading ~A~%" file)
                  (#"read" model 
                   (concatenate 'string 
                                "file:///"
                                (namestring file))
                   "TTL")))
            turtle-files)))




(defun sparql-construct (query &optional (model *model*))
  "returns a model"
  (let* ((query-execution (get-query-execution query model))
         (result-model (#"execConstruct" query-execution)))
    result-model))

; dont export
(defun concat-prefixlist (lis)
  (reduce #'(lambda (x y)
              (concatenate 'string (format nil "~A~%" x) 
                           (format nil "~A~%" y)))
          lis))

; dont export
(defun get-update-processor (query &optional (dataset *ds*))
  (let* ((prefixes (concat-prefixlist *prefix-list*))
         (update-request (#"add" 
                          (jss:new "org.apache.jena.update.UpdateRequest")
                          (format nil "~A ~A" prefixes query))))
    (jstatic "create"
             "org.apache.jena.update.UpdateExecutionFactory"
             update-request
             dataset)))


; dont export
(defun get-query-execution (query &optional (model *model*))
  (let* ((prefixes (concat-prefixlist *prefix-list*))
         (query-execution (jstatic "create"
                                   (jclass "org.apache.jena.query.QueryExecutionFactory")
                                   (format nil "~A ~A" prefixes query)
                                   model)))
    query-execution))


(defun get-result-set (result-set)
  (let* ((alists ; one alist per binding set
           (loop :while (#"hasNext" result-set)
                 :collect (let ((query-solution (#"next" result-set)))
                            (let* ((vars (#"varNames" query-solution)))
                              (loop :while (#"hasNext" vars)
                                    :collect (let* ((var (#"next" vars)))
                                               (cons var
                                                     (extract-value
                                                      (#"get" query-solution
                                                       var))))))))))
    alists))

; dont export
(defun extract-value (rdf-node)
  (if (#"isLiteral" rdf-node)
      (#"getValue" rdf-node)
      (#"toString" rdf-node)))

(defun print-alists (alists)
  (dolist (alist alists)
    (progn
      (dolist (binding alist) 
        (format t "~A: ~A~%" (car binding) (cdr binding)))
      (format t "~%"))
    alist))


(defun sparql-update (query &optional (dataset *ds*))
  (let* ((update-processor (get-update-processor query dataset)))
         (unwind-protect 
           (progn
             (#"begin" dataset) ; TODO use the general start txn function?
             (#"execute" update-processor)
             (#"commit" dataset))
           (#"end" dataset))))


(defun sparql-select (query &optional (dataset *ds*))
  (let* ((model (#"getDefaultModel" dataset))
         (query-execution (get-query-execution query model))
         (resultset (#"execSelect" query-execution)))
    (unwind-protect 
      (progn
        (start-txn dataset)
        ;(loop :while (#"hasNext" resultset) :do (let ((result (#"next" resultset))) (format t "~A~%" (#"toString" result))))
        (get-result-set resultset))
      (end-txn dataset))))



; TODO i think i need the type e.g ds.begin(ReadWrite.READ);
(defun start-txn (dataset)
  (when (not (null dataset))   
    (jcall "begin"
           dataset)))

(defun end-txn (dataset)
  (when (not (null dataset))   
    (jcall "end"
           dataset)))
  

; TODO need to accept *ds* to start the read txn
; TODO need to handle the case when there is no dataset (just memorymodel)
(defun model->jsonld-embedded (&optional (stream t) (model *model*) (dataset *ds*))
  (unwind-protect
    (progn
      (#"begin" dataset)
      (let* ((writer (jss:new "org.apache.jena.riot.writer.JsonLDWriter" 
                              (jfield "org.apache.jena.riot.RDFFormat"
                                      "JSONLD_FRAME_PRETTY")))
             (datasetgraph (#"asDatasetGraph"
                            (jstatic "wrap" (jclass "org.apache.jena.query.DatasetFactory")
                                     model)))
             (os (jss:new "java.io.ByteArrayOutputStream"))
             (ctx (jss:new "org.apache.jena.riot.JsonLDWriteContext"))
             (frame (jss:new "java.util.HashMap")))
        (if (#"isEmpty" model)
            (format stream "")
            (progn 
              (#"put" frame "@embed" "@always")
              (#"setFrame" ctx frame)
              (#"write" writer
               os
               datasetgraph
               +null+
               +null+ 
               ctx)
              (format stream "~A" (#"toString"
                                   (jss:new "java.lang.String" 
                                            (#"toByteArray" os))
                                   ))))))
    (#"end" dataset)))


(defun read-from-file-into-dataset (file-path &key (dataset *ds*) (file-format "TTL"))
  "file-format options:
   https://github.com/apache/jena/blob/master/jena-core/src/main/java/org/apache/jena/rdf/model/impl/RDFReaderFImpl.java
   \"RDF\" \"RDF/XML\" \"RDF/XML-ABBREV\" \"N-TRIPLE\" \"N-TRIPLES\" \"N-Triples\" \"N3\" \"TURTLE\" \"Turtle\" \"TTL\""
  (let* ((model (#"getDefaultModel" dataset))
         (file-uri (concatenate 'string
                                "file:///"
                                file-path)))
    (unwind-protect
      (progn
        (#"begin" dataset)
        (#"read" model 
         file-uri
         file-format))
      (#"commit" dataset))))


; TODO assumes turtle file
(defun write-dataset-to-file (file-path &key (dataset *ds*))
  (let* ((file-output-stream (jss:new "java.io.FileOutputStream"
                                      (jss:new "java.io.File"
                                               file-path)))
         (model (#"getDefaultModel" dataset)))
    (unwind-protect 
      (progn
        (#"begin" dataset)
        (jstatic "write"
                 "org.apache.jena.riot.RDFDataMgr"
                 file-output-stream
                 model
                 (jfield 
                   "org.apache.jena.riot.RDFFormat"
                   "TURTLE_PRETTY")))
      (#"end" dataset))))
