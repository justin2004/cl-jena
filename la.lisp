(ql:quickload :cl-jena)
(java:add-to-classpath "/home/containeruser/quicklisp/shared-things-1.0.0-jar-with-dependencies.jar")
(elt  (java:dump-classpath)
      0)

(in-package :jena)

;ReasonerRegistry.getOWLReasoner ()

(jss:japropos "ReasonerRegistry")
(jstatic "getOWLReasoner"
         "org.apache.jena.reasoner.ReasonerRegistry")


(jena::get-default-model)
(setf *ds* nil)

*ds*

(jena::make-owl-reasoner)
(setf *owl-reasoner* *)

(setf *infm*
      (jena::make-inf-model *ds*
                  (jena::make-owl-reasoner)))

(setf *infm* *)
(#"rebind"
 *infm*)

(jena::print-alists
  (sparql-select "select * where {d2rq:henry ?p ?o}" :dataset *ds*  ))

(jena::print-alists
  (sparql-select "select * where {d2rq:henry ?p ?o}" :dataset *ds*
                 :input-model *infm*))

(jena::print-alists
  (sparql-select "select * where {?s ?p d2rq:person}" :input-model *infm* ))

(jena::print-alists
  (sparql-select "select * where {d2rq:fred ?p ?o}" :input-model *infm* ))

(jena::print-alists
  (sparql-select "select (count(?s) as ?cnt)
                  where {?s ?p ?o}" :input-model *infm* ))

(jena::print-alists
  (sparql-select "select * where {?s ?p ?o}" :input-model *infm* ))

(sparql-construct "construct {?s ?p ?o} where {?s ?p ?o}" *ds*)
(setf *somemodel* *)

(sparql-select "select (count(?s) as ?cnt) where {?s ?p ?o}" :input-model *infm* )



(sparql-update "insert data { 
                 d2rq:child a owl:class .
                 d2rq:person a owl:class .
                 d2rq:child rdfs:subClassOf d2rq:person .
                 d2rq:bob a d2rq:person .
                 d2rq:henry a d2rq:child .
                }" 
               :dataset *ds*
               :inf-model *infm*)

(sparql-update "insert data { d2rq:person a owl:class}" 
               :dataset *ds*
               :inf-model *infm*)
(sparql-update "insert data { d2rq:bob a d2rq:person}" 
               :dataset *ds*
               :inf-model *infm*)
(sparql-update "insert data { d2rq:fred a d2rq:child}" 
               :dataset *ds*
               :inf-model *infm*)

(sparql-update "insert data { d2rq:child rdfs:subClassOf d2rq:person}" 
               :dataset *ds*
               :inf-model *infm*)


(setf *res* *)

(mapcar #'(lambda (alis)
            (let* ((pair (assoc "o"
                                alis
                                :test #'string=
                                )))
              (when pair
                (cdr pair))))
        *res*)

(mapcar #'(lambda (alis)
            (when (rassoc ".*fred.*"
                    alis
                    :test #'(lambda (regx target)
                              (when (stringp target) 
                                (ppcre:scan regx
                                          target))))
              alis))
        *res*)

*res*

(ql:quickload "cl-ppcre")

(ppcre:scan ".*fred"
            "ajsjdjsd#freD")

(equalp 2 3)


(diff *infm* *ds*)

(#"begin" *ds*)

(#"difference" 
 (#"getDefaultModel" *ds*)
 *infm*)
(setf *b* *)

(#"difference" *infm*
 (#"getDefaultModel" *ds*))
(setf *a* *)

(#"end" *ds*)

(jena::print-alists (sparql-select "select * where {?s ?p ?o}" 
               :dataset *ds*
               :input-model *b* ))









;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; example of using owl inference

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
; he is also a person let's look in the inference model

; look for statements about henry in the infernece model again
(jena::print-alists
  (sparql-select "select * where {d2rq:henry ?p ?o}" 
                 :dataset *ds*
                 :input-model *infm*))

; p: http://www.w3.org/1999/02/22-rdf-syntax-ns#type
; o: http://www.wiwiss.fu-berlin.de/suhl/bizer/D2RQ/0.1#child
; 
; p: http://www.w3.org/1999/02/22-rdf-syntax-ns#type
; o: http://www.wiwiss.fu-berlin.de/suhl/bizer/D2RQ/0.1#person

;; and now you can see henry is also a person




;;;;;;;;;;;;;;;;;;;;;;

; (jena::make-owl-reasoner)
; (setf *r* *)
(setf *ds* nil)
(jena::get-default-model)
(setf *infm*
      (jena::make-inf-model *ds*
                  (jena::make-owl-reasoner)))

(sparql-update "insert data { 
                 d2rq:child rdfs:subClassOf d2rq:person .
                 d2rq:real-boy a owl:Class ;
                         owl:intersectionOf (d2rq:child d2rq:biological-thing) .
                d2rq:bob a d2rq:child .
                }" 
               :dataset *ds*
               :inf-model *infm*)

(jena::print-alists
  (sparql-select "select * where {
                  ?s a owl:AllDifferent .
                  ?s ?p ?o .
                  }" :dataset *ds*
                 :input-model *infm*))


(sparql-update "insert data {
                d2rq:solarP a owl:Class .
                d2rq:mercury owl:differentFrom d2rq:venus .
                d2rq:mercury owl:differentFrom d2rq:earth .
                d2rq:venus owl:differentFrom d2rq:earth .
                d2rq:solarP owl:oneOf ( d2rq:mercury d2rq:venus d2rq:earth ) .
                d2rq:lump0 a owl:Class .
                d2rq:lump0 a d2rq:solarP .
                }
                " 
               :dataset *ds*
               :inf-model *infm*)

(sparql-update "insert data {
                d2rq:solarP a owl:Class ;
                            owl:equivalentClass [ a owl:Restriction ;
                                                    owl:onProperty d2rq:hasMass ;
                                                    owl:hasValue \"12 units\" ] .

                [ a owl:AllDifferent ;
                    owl:distinctMembers (d2rq:mercury 
                                          d2rq:venus 
                                          d2rq:earth )
                    ] .
                d2rq:solarP owl:oneOf ( d2rq:mercury d2rq:venus d2rq:earth ) .
                d2rq:lump0 a owl:Class .
                d2rq:lump0 a d2rq:solarP .
                d2rq:lump0 rdfs:comment \"justin comment\" .
                d2rq:lump0 owl:differentFrom d2rq:mercury .
                d2rq:lump0 owl:differentFrom d2rq:venus .
                }
                " 
               :dataset *ds*
               :inf-model *infm*)



(sparql-update "insert data {
                [ a owl:AllDifferent ;
                    owl:distinctMembers (d2rq:mercury 
                                          d2rq:venus 
                                          d2rq:earth )
                    ] .
                }
                " 
               :dataset *ds*
               :inf-model *infm*)


(sparql-update "insert data {
                d2rq:mercury owl:differentFrom d2rq:venus .
                d2rq:mercury owl:differentFrom d2rq:earth .
                d2rq:venus owl:differentFrom d2rq:earth .
                }
                " 
               :dataset *ds*
               :inf-model *infm*)

(sparql-construct "construct {?s ?p ?o} where {?s ?p ?o}"
                  :dataset *ds*
                  :input-model *infm*)

(setf *mod* *)
(sparql-select "select * where {?s ?p ?o}"
               :dataset *ds*
               :input-model *mod*)


;;;;;;;;;;
; card

(setf *ds* nil)
(jena::get-default-model)
(setf *infm*
      (jena::make-inf-model *ds*
                            (jena::make-owl-reasoner)))

(jena::make-micro-owl-reasoner)




(jena:read-from-file-into-dataset "/mnt/example.ttl" :dataset *ds*)
(jena:read-from-file-into-dataset "/mnt/my.ttl" :dataset *ds*)
*prefix-list*
(push "prefix f: <http://example.com/owl/families#>" 
      *prefix-list*)
(push "prefix : <http://www.semanticweb.org/justin/ontologies/2020/5/untitled-ontology-3#>" 
      *prefix-list*)

(#"rebind" *infm*)
(jena::print-alists
  (sparql-select "select * where {f:Sal a ?o .
                  }" :dataset *ds* :input-model *infm*  ))

(sparql-update "insert data { 
                f:Sal f:hasGender f:male .
                f:Sal f:hasGender f:female .
                f:Sal f:hasAge 30 .
                }" 
               :dataset *ds*
               :inf-model *infm*)
(sparql-update "delete {f:Sal f:hasAge 30 } where { 
                      f:Sal f:hasAge 30
                }" 
               :dataset *ds*
               :inf-model *infm*)

(sparql-update "insert data { 
                :Mets :hasPlayer :Bob .
                :Mets :hasPlayer :Fred .
                :Mets :hasPlayer :Alvin .
                :Bob a owl:Thing . 
                :Fred a owl:Thing . 
                :Alvin a owl:Thing . 
                :Bob owl:differentFrom :Fred .
                :Bob owl:differentFrom :Alvin .
                :Fred owl:differentFrom :Alvin .
                }" 
               :dataset *ds*
               :inf-model *infm*)

(sparql-update "insert data { 
                d2rq:mets a owl:Class ;
                          d2rq:hasPlayer d2rq:fred , d2rq:bob . 
                d2rq:hasPlayer a owl:ObjectProperty .
                [ a owl:AllDifferent ;
                    owl:distinctMembers (d2rq:bob d2rq:fred) ] .
                }" 
               :dataset *ds*
               :inf-model *infm*)

(sparql-update "insert data { 
                d2rq:team a owl:Class ;
                          rdfs:subClassOf [ a owl:Restriction ;
                                        owl:onProperty d2rq:hasPlayer ;
                                        owl:cardinality \"2\"^^xsd:nonNegativeInteger ] .
                }" 
               :dataset *ds*
               :inf-model *infm*)


(sparql-update "insert data { 
                d2rq:mets d2rq:hasPlayer d2rq:bob .
                d2rq:mets d2rq:hasPlayer d2rq:fred .
                d2rq:bob a owl:Thing . 
                d2rq:fred a owl:Thing . 
                }" 
               :dataset *ds*
               :inf-model *infm*)


;;;;;;;;;;;;;;;;;;;;;;;;;;;

; String rules = "[rule1: (?a eg:p ?b) (?b eg:p ?c) -&gt; (?a eg:p ?c)]";
; Reasoner reasoner = new GenericRuleReasoner(Rule.parseRules(rules));
; reasoner.setDerivationLogging(true);
; InfModel inf = ModelFactory.createInfModel(reasoner, rawData);

(jss:java-class-method-names  "org.apache.jena.reasoner.rulesys.GenericRuleReasoner")
(jss:java-class-method-names  "org.apache.jena.reasoner.rulesys.GenericRuleReasoner")
(setf *infm-prime* (jstatic "createInfModel" 
                            "org.apache.jena.rdf.model.ModelFactory"
                            *owl-reasoner*
                            *infm*))
(setf *infm0* (jstatic "createInfModel" 
                      "org.apache.jena.rdf.model.ModelFactory"
                      *owl-reasoner*
                      *model*))
(setf *infm* (jstatic "createInfModel" 
                      "org.apache.jena.rdf.model.ModelFactory"
                      *g-reasoner*
                      *infm0*))
; trying with multiple keys
(setf *g-reasoner* (jss:new  "org.apache.jena.reasoner.rulesys.GenericRuleReasoner"
          (jstatic "parseRules" 
                   "org.apache.jena.reasoner.rulesys.Rule"
                   "[rule1: 
                    (?restriction <http://www.w3.org/2020/01/justin#hasKey> ?key) 
                    (?ec owl:equivalentClass ?restriction) 
                    (?s rdf:type ?ec)
                    (?s ?key ?keyvalue)
                    strConcat(?key,?keyAsString)
                    regex(?keyAsString,'.*[#/]([^#/]*)$',?cap)
                    uriConcat(?ec, \"/asIdentifiedBy/\", ?cap, \"/\" , ?keyvalue, ?uri)
                    -> 
                    print(?uri,\"rule fired\")
                    (?s owl:sameAs ?uri)
                    ]")))
(setf *g-reasoner* (jss:new  "org.apache.jena.reasoner.rulesys.GenericRuleReasoner"
          (jstatic "parseRules" 
                   "org.apache.jena.reasoner.rulesys.Rule"
                   "[rule1: 
                    (?restriction <http://www.w3.org/2020/01/justin#hasKey> ?key) 
                    (?ec owl:equivalentClass ?restriction) 
                    (?s rdf:type ?ec)
                    (?s ?key ?keyvalue)
                    strConcat(?key,?keyAsString)
                    strConcat(?keyvalue,?keyvalueAsString)
                    regex(?keyAsString,'.*[#/]([^#/]*)$',?cap)
                    uriConcat(?ec,\"/blank-nodes/\", \"asIdentifiedBy/\", ?cap, \"/\" , ?keyvalueAsString, ?uri)
                    makeSkolem(?skolem,?ec,?cap,?keyvalueAsString)
                    strConcat(?skolem,?skolem-string)
                    uriConcat(?ec,\"/blank-nodes-with-makeskolem/\", \"asIdentifiedBy/\", ?cap, \"/\" , ?skolem, ?uri-skolem)
                    -> 
                    print(?uri,\"rule fired\")
                    (?s owl:sameAs ?uri-skolem)
                    ]")))
; in order to do this                   
; (?s a ?ec)
; we'd have to run the owl reasoner before the swrl rules

; wasn't working
;                    makeSkolem(?uri, ?ec, ?key, ?keyvalue)

; not using these anymore in the head -- just want the uri derived from the skolem
; (?s owl:sameAs ?uri)
; (?s owl:sameAs ?skolem)


*prefix-list*


(#"parseRules"  "org.apache.jena.reasoner.rulesys.Rule")
(java:jclass-methods  "org.apache.jena.reasoner.rulesys.Rule")

(java:jcall "dd" 'e)

(print-alists (sparql-select "select * where 
                              {
                              ?s owl:sameAs ?o .
                              filter(?s = :al || ?s = :ally || ?s = :albert || ?s = :alber) .
                              } "
                             :input-model *infm-prime*))

(print-alists (sparql-select "select * where 
                              {
                              ?res justin:hasKey ?key .
                              ?ec owl:equivalentClass ?res .
                              ?s a ?ec .
                              ?s ?key ?keyvalue .
                              } "
                             :input-model *model*))

(sparql-select-vim "select * where {?s ?p ?o}"
                   :input-model *infm-prime*)

(push "prefix justin: <http://www.w3.org/2020/01/justin#>"
      *prefix-list*)
*prefix-list*


(sparql-update "insert data {
                :al a :Blah .
                :al <http://www.semanticweb.org/justin/ontologies/2020/5/untitled-ontology-3#hasSomeID> 99 .
                :al <http://www.semanticweb.org/justin/ontologies/2020/5/untitled-ontology-3#hasname> \"al\" .
                :al <http://www.semanticweb.org/justin/ontologies/2020/5/untitled-ontology-3#hasAnotherID> 150 .
                :al <http://www.semanticweb.org/justin/ontologies/2020/5/untitled-ontology-3#hasAnotherID> \"wordy words\" .
                :al <http://www.semanticweb.org/justin/ontologies/2020/5/untitled-ontology-3#hasSomeID> <http://google.com/3> .
                :albert a :Blah .
                :albert <http://www.semanticweb.org/justin/ontologies/2020/5/untitled-ontology-3#hasSomeID> 99 .
                :albert <http://www.semanticweb.org/justin/ontologies/2020/5/untitled-ontology-3#hasSomeID> <http://google.com/99> .
                :alber a :Blah .
                :alber <http://www.semanticweb.org/justin/ontologies/2020/5/untitled-ontology-3#hasSomeID> <http://google.com/99> .
                <http://google.com/100> owl:sameAs <http://google.com/99> .
                :ally a :Blah .
                :ally <http://www.semanticweb.org/justin/ontologies/2020/5/untitled-ontology-3#hasSomeID> <http://google.com/3> .
                :ally <http://www.semanticweb.org/justin/ontologies/2020/5/untitled-ontology-3#hasSomeID> <http://google.com/100> .
                :ally <http://www.semanticweb.org/justin/ontologies/2020/5/untitled-ontology-3#hasname> \"ally\" .
                }"
               :inf-model *infm-prime*)








 ; write (OutputStream out, String lang))
(#"write" (sparql-construct "construct {?s ?p ?o} where
                             {
                             ?s ?p ?o .
                             filter(?s = :al || ?s = :ally) .
                             } "
                            :input-model *infm*)
          (jfield "java.lang.System" 
                  "out")
          "TTL")

(jfield "java.lang.System" 
        "out")

;; notes
; it seems like if you see ?s owl:sameAs ?o
; then you need to (1) run owl (to carry out sameAs semantics for justin:hasKey to use),
;                  (2)  then swrl (to carry out justin:hasKey semantics), 
;                  (3)  then owl (to carry out sameAs semantics to use what justin:hasKey semantics did)
; and if the output includes a new ?s owl:sameAs ?o
; then you need to do the 3 again

; (1) can be done on distinct datasets
; (2) can be done a subsets of a big distinct dataset (because of makeskolem)
;        as long as all statements about a subject are all in the same subset
; (3) can be done on any subset (because of makeskolem distinct invocations of reasoners
;     will cooperate because of the URIs already made
