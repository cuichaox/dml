
(defsystem :dml
    :description "Diagram Make Language for common lisp"
    :name        "dml"
    :version     "0.1"
    :licence     "MIT License"
    :author      "cuic@chianre.com.cn"
    :depends-on  (:donuts :cl-cairo2 :alexandria :cl-ppcre)
    :components  ((:file "src/dml.graph")
                  (:file "src/dml.node")                  
                  (:file "src/dml.seq.grid")
                  (:file "src/dml.seq.core")                         
                  (:file "src/dml.seq.engine"
                         :depends-on ("src/dml.seq.core" "src/dml.seq.grid"))
                  (:file "src/dml"
                         :depends-on ("src/dml.graph")))) 
                 
