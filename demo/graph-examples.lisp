(in-package :dml)

;; Example 1
($ (:outfile "dml.png")
   (& (:rankdir :LR)
     (-dep-- "use"
             (comp "DML")
             (comp "donuts")
             (comp "Graphviz"))))

;; Example 2
($ (:outfile "class.png")
   (with-method ("+ play () : Love" "+ work () : Hate")
      (-genby-* (full-class "OS"
                            "abstract"
                            (attributes "- name : String"
                                        "- born : Date"))
                               
                (full-class "Linux")
                (full-class "Android")
                (full-class "Apple")
                (full-class "Windows"))
      (-dep- "from" (@name "Android")
                    (@name "Linux"))))

;;Example 3
($ (:outfile "ucase.png")
   (& (:rankdir :LR)
      (->> (actor "user")
           (ucas "Use Emacs")
           (ucas "Programing")
           (ucas "Play game"))
      (-dep- "extend"
             (@name "Programing")
             (@name "Use Emacs"))))

