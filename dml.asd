
(defsystem :dml
    :description "Diagram Make Language for common lisp"
    :name        "dml"
    :version     "0.1"
    :licence     "MIT License"
    :author      "cuic@chianre.com.cn"
    :depends-on  (:donuts :cl-cairo2 :alexandria)
    :serial t
    :components  ((:file "src/dml-seq")
                  (:file "src/dml")
                  (:static-file "src/actor.png")))
