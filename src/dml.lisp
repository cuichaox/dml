
(uiop:define-package :dml
    (:mix #:dml.graph
          #:dml.seq.core
          #:dml.seq.engine)
  (:export   #:dml-create-graph
             #:dml-create-sequnce
             #:attributes
             #:methods
             #:@name
             #:full-class
             #:simp-class
             #:actor
             #:ucas
             #:pack
             #:comp           
             #:-genby-
             #:-genby-*
             #:-relby-
             #:-relby-*
             #:-com-
             #:-com-*
             #:-agg-
             #:-agg-*
             #:-dep-
             #:-dep-*
             #:->>
             #:-dep--))

(in-package :dml)

(defmacro dml-create-graph (path-name-prefix (&rest graph-attrs) &body nodes-edges-graphs)
  `(let ((graph (& ,graph-attrs ,@nodes-edges-graphs)))
     ($ (:outfile ,(concatenate 'string path-name-prefix ".ps") :show nil)                  
        graph)
     ($ (:outfile ,(concatenate 'string path-name-prefix ".png") :show t)
        graph)))
 
(defmacro dml-create-sequnce (path-name-prefix (&rest sequnce-attrs) &body any-message))
  
