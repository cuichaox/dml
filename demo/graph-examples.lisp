;;;; examples  for graph exampels

(use-package :dml)

(uiop:chdir (asdf:system-relative-pathname :dml "demo"))

(dml-create-graph "dml-component" (:rankdir :LR)
  (-dep-- "use"
          (comp "DML")
          (comp "donuts")
          (comp "Graphviz")))

(dml-create-graph "os-class" ()
  (with-method ("+ play () : Love" "+ work () : Hate")
    (-genby-*
     (full-class "OS"
                 "abstract"
                 (attributes "- name : String"
                             "- born : Date"))
     (full-class "Linux")
     (full-class "Android")
     (full-class "Apple")
     (full-class "Windows"))
    (-dep- "from"
           (@name "Android")
           (@name"Linux"))))

(dml-create-graph "coder-case" (:rankdir :LR)
  (->> (actor "coder")
       (ucas "Use Emacs")
       (ucas "Programing")
       (ucas "Play game"))
  (-dep- "extend"
         (@name "Programing")
         (@name "Use Emacs")))

(dml-create-graph "customer" (:rankdir :LR)
  (->> (actor "客户")
       (ucas "添加案例")       
       (ucas "浏览历史案例"))
  (-genby-* (@name "添加案例")
            (ucas "创建新案例")
            (ucas "复制旧案例"))
  (-dep- "extend"
         (ucas "下载/邮寄PDF(案例报价)")
         (ucas "查看报价结果"))
  (-dep- "extend"
         (@name "复制旧案例")
         (@name "浏览历史案例"))
  (-dep- "extend"
         (@name "查看报价结果")
         (@name "浏览历史案例"))
  (-dep- "extend"
         (ucas "删除案例")
         (@name "浏览历史案例"))
  (-dep- "include"
         (@name "添加案例")
         (@name "查看报价结果"))
  (-dep-- "include"
          (@name "添加案例")
          (ucas "录入方案")
          (ucas "录入人员信息"))          
  (-genby-* (@name "录入人员信息")
            (ucas "输入总人数")
            (ucas "输入各个年龄段人数")
            (ucas "导入人员明细")))         
           
(dml-create-graph "dom" ()
  (with-method ("+ calcPrice(): BigDecimal"
                "+ toJson(): String"
                "+ getValue(name:String) : Object")
    (-genby-* (full-class "TreeNode"
                          "Abstract"
                          nil
                          (methods "+ getParent(): TreeNode"
                                   "+ addChild(:TreeNode)"))
              (full-class "PriceCache" "Decorator"
                          (attributes "- theNode : TreeNode"))
              (full-class "Case")
              (full-class "Plan")
              (full-class "Categorization"
                          nil
                          (attributes "- fromAge : integer"
                                      "- toAge : integer"
                                      "- isSheBao : boolean"
                                      "- count : integer"))))
  (-dep- "Create"
         (full-class "CaseBuilder"
                     "Factory"
                     nil (methods "+ parseJson(: String) : Case"))                                  
         (@name "Case"))
  (-agg- (@name "TreeNode")
         (@name "TreeNode"))
  (-com- (@name "PriceCache")
         (@name "TreeNode"))
  (-com- (@name "Case")
         (@name "Plan"))
  (-com- (@name "Plan")
         (@name "Categorization"))  
  (-dep- "Usage"
         (@name "Categorization")
         (full-class "Calculator"
                     "Utility"
                     nil
                     (methods "+ calc(arg : TreeNode) : BigDecimal"
                              "- log2db() :")))
  
  (with-method ("+ loadFromdb()"
                "+ lookUp()")
    (-com- (@name "Calculator")
           (full-class "ArgumentTable"))
    (-dep- "Call"
           (@name "Calculator")
           (@name "TreeNode"))  
    (-genby-* (@name "ArgumentTable")
              (full-class "ByAgeScope")
              (full-class "ByAge")
              (full-class "ByID"))
    (-dep- "Usage" (@name "ArgumentTable")
           (pack "JDBC"))))
