
(uiop:define-package :dml
    (:mix #:dml.graph
          #:dml.seq.core
          #:dml.seq.engine
          #:cl)
  (:export   #:dml-create-graph
             #:dml-create-sequnce
             #:&go
             #:&in
             #:&opt
             #:&if
             #:&loop
             #:attributes
             #:methods
             #:with-method
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
  "Dump graph diagram to files (ps & png)."
  `(let ((graph (& ,graph-attrs ,@nodes-edges-graphs)))
     ($ (:outfile ,(concatenate 'string path-name-prefix ".ps") :show nil)                  
        graph)
     ($ (:outfile ,(concatenate 'string path-name-prefix ".png") :show nil)
        graph)))
 
(defmacro dml-create-sequnce (path-name-prefix (&rest sequnce-attrs) &body any-message)
  "Dump sequnce diargram to files (ps & png)."
  `(let ((*context-objects* nil)
         (*context-sequnce-attrs* (append ,sequnce-attrs *context-sequnce-attrs*)))
     (make-sequnce-diagram ,path-name-prefix ,@any-message)))
  
