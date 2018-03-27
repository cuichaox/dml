(use-package :dml)

(uiop:chdir (asdf:system-relative-pathname :dml "demo"))

(dml-create-graph "dml-component" (:rankdir :LR)
  (-dep-- "use" (comp "DML") (comp "donuts") (comp "Graphviz")))

(dml-create-graph "os-class" ()
  (with-method ("+ play () : Love" "+ work () : Hate")
    (-genby-* (full-class "OS" "abstract"
                          (attributes "- name : String" "- born : Date"))
              (full-class "Linux") (full-class "Android")
              (full-class "Apple") (full-class "Windows"))
    (-dep- "from" (@name "Android") (@name"Linux"))))

(dml-create-graph "coder-case" (:rankdir :LR)
  (->> (actor "coder")
       (ucas "Use Emacs") (ucas "Programing") (ucas "Play game"))
  (-dep- "extend" (@name "Programing") (@name "Use Emacs")))


