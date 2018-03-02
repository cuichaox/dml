(in-package :donuts)

;; Example 1
($ (:outfile "dml.png")
   (& (:rankdir :LR)
     (-dep-- "use"
             (comp "DML")
             (comp "donuts")
             (comp "Graphviz"))))

;; Example 2
($ (:outfile "class.png")
   (&& (-genby-* (full-class "OS"
                            "abstract"
                            (attributes "- name : String"
                                        "- born : Date")
                            (methods "+ play ()"
                                     "+ work ()"))
                 (full-class "Linux")
                 (full-class "Apple")
                 (full-class "Windows"))))

                                     
  
