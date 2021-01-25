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
(require :cl-csv) ; TODO do i need this?

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


; creates a tdb2 dataset
(defun get-default-dataset (&optional &key (in-memory t))
  (get-default-model :in-memory in-memory))

(defun get-default-model (&optional &key (in-memory t))
  "sets *model* and *ds* to default model and the corresponding dataset, respectively.
   the dataset can either be in memory or on disk"
  (if (null *ds*)
      ; start from scratch
      (let* ((file-based-dataset (jstatic "connectDataset" (jclass "org.apache.jena.tdb2.TDB2Factory")
                                          (directory-namestring (ensure-directories-exist "/tmp/some_tdb2dataset/"))))) 
        (progn
          (setf *ds*
                (if in-memory
                    (jstatic "createDataset" (jclass "org.apache.jena.tdb2.TDB2Factory"))
                    file-based-dataset))
        (setf *model* (#"getDefaultModel" *ds*))))
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



(defun sparql-construct (query &key (dataset *ds*) input-model)
  "returns a model ; TODO maybe this should return a dataset to fit with the pattern here
   if input-model is provided it is used"
  (let* ((model (if (null input-model)
                    (#"getDefaultModel" dataset)
                    input-model))
         (query-execution (get-query-execution query model)))
    (unwind-protect
      (progn
        (#"begin" dataset)
        (#"execConstruct" query-execution)) ; returns a model
      (progn
        (#"close" query-execution)
        (#"end" dataset)))))

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

(defun print-alists-csv (alists &optional 
                                (stream *standard-output*))
  "print a list of alists as .csv 

   assumes that each alist has the same keys as all the others
   "
  ; header
  ;   use the first alist to get the keys
  (let* ((first-alist (car alists))
         (varbs (mapcar #'(lambda (alist)
                            (car alist))
                        first-alist))
         (sorted-varbs (sort varbs
                             #'string<)))
    (cl-csv:write-csv (list sorted-varbs) 
                      :stream stream))
  ; rows
  (dolist (alist alists)
    (let* ((sorted-alist (sort alist
                               #'string<
                               :key #'car))
           (just-values (mapcar #'(lambda (binding)
                                    (cdr binding))
                                sorted-alist)))
      (cl-csv:write-csv (list just-values)
                        :stream stream))))



; TODO does this need the inf-model if we are using one?
;      e.g. would a delete insert statement need to see the result of inference?
(defun sparql-update (query &key (dataset *ds*) inf-model)
  "pass in an inf-model if you want it to take into account the changes you are making in the dataset"
  (let* ((update-processor (get-update-processor query dataset)))
    (unwind-protect 
      (progn
        (#"begin" dataset) ; TODO use the general start txn function?
        (#"execute" update-processor)
        (#"commit" dataset))
      (progn
        (#"end" dataset)
        ; if an inf-model is given then tell the "inference model to reconsult the underlying data to take into account changes."
        (when inf-model
          (#"rebind" inf-model))))))



(defun sparql-select (query &key (dataset *ds*) input-model)
  "if you provide an input-model (on top of the dataset) it will be used"
  (let* ((model (if (null input-model) 
                    (#"getDefaultModel" dataset)
                    input-model))
         (query-execution (get-query-execution query 
                                               model))
         (resultset (#"execSelect" query-execution)))
    (unwind-protect 
      (progn
        (start-txn dataset)
        ;(loop :while (#"hasNext" resultset) :do (let ((result (#"next" resultset))) (format t "~A~%" (#"toString" result))))
        (get-result-set resultset))
      (end-txn dataset))))


; this isn't only for vim users but the default format-string
;   is vim-centric
(defun sparql-select-vim (query &key 
                                (dataset *ds*)
                                input-model
                                (format-string "!vd ~A --filetype=csv"))
  "runs jena:sparql-select, converts the resultant output to csv,
   puts it into a temporary file,
   uses the provided :format-string to print a command to the screen you 
   can use to view the output.

   :format-string    this is where you put the program you want to view the csv with.
                     needs one ~A which will become the temporary file name.

   e.g.
   ; if you want to look at the output in visidata (vd)

   (sparql-select-vim \"select * where {?s ?p ?o}\" 
                      :format-string \":!vd ~A --filetype=csv\")

   ; which prints a vim ex command to the screen that you can execute.
   ; see https://github.com/justin2004/weblog/tree/master/lisp_sparql_visidata for more detail on doing that in vim.

   "

  (let* ((temp-file (uiop:run-program "mktemp" :output '(:string :stripped t))))
    (with-open-file (s temp-file
                       :if-does-not-exist :create 
                       :direction :output)
      (jena::print-alists-csv (jena:sparql-select query
                                                  :dataset dataset
                                                  :input-model input-model)
                              s))
    ; print the string to stdout
    (format t format-string temp-file)

    ; this works-ish but in slimv_box you have to switch to the REPL window in tmux
    ; and then it seems like some character presses get consumed before visidata
    ; get them so usage is not responsive
    ;(uiop:launch-program  (format nil "vd ~A --filetype=csv" temp-file) 
    ;                      :output :interactive)
    ))



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
(defun write-dataset-to-file (file-path &key (dataset *ds*) (input-model nil))
  (let* ((file-output-stream (jss:new "java.io.FileOutputStream"
                                      (jss:new "java.io.File"
                                               file-path)))
         (model (if (null input-model) 
                    (#"getDefaultModel" dataset)
                    input-model)))
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



;;;; reasoning


; owl micro
(defun make-micro-owl-reasoner ()
  (let* ((owl-config (#"createResource"
                      (jstatic "createDefaultModel"
                               "org.apache.jena.rdf.model.ModelFactory")))
         (owl-reasoner (#"create"
                        (jstatic "theInstance"
                                 "org.apache.jena.reasoner.rulesys.OWLMicroReasonerFactory")
                        owl-config)))
    owl-reasoner))

; owl mini
(defun make-mini-owl-reasoner ()
  (jstatic "getOWLMiniReasoner"
           "org.apache.jena.reasoner.ReasonerRegistry"))

; owl full
(defun make-owl-reasoner ()
  (jstatic "getOWLReasoner"
           "org.apache.jena.reasoner.ReasonerRegistry"))


; TODO does not support inf-models on inf-models
(defun make-inf-model (input-dataset reasoner)
  (let* ((inf-model (jstatic "createInfModel"
                             "org.apache.jena.rdf.model.ModelFactory"
                             reasoner
                             (#"getDefaultModel" input-dataset))))
    inf-model))



; TODO defgeneric
; TODO assumes dataset underlies the model
(defun diff (model dataset)
  (unwind-protect
    (progn
      (#"begin" dataset)
      (values
        (#"difference" model
         (#"getDefaultModel" dataset))
        (#"difference" (#"getDefaultModel" dataset)
         model)))
    (#"end" dataset)))

; TODO instead of the diff function above maybe implment a ModelChangedListener
; https://jena.apache.org/documentation/javadoc/jena/index.html?org/apache/jena/rdf/model/ModelFactory.html
; so we can print statements that are derived as a result of updating a model
