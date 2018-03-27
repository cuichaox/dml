
(uiop:define-package :dml.graph
    (:mix #:cl
          #:donuts
          #:dml.node)
  (:export  #:$
            #:&
            #:with-method
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
            #:->>
            #:-dep-*
            #:-dep--))
           

(in-package :dml.graph)

;;把列表转换成多行label
(defun multi-rows-label (labels)
  (let ((ret ""))
    (dolist (row labels)
      (setf ret (concatenate 'string ret (if (consp row) (car row) row) "\\l")))
    ret))

;;原型标签
(defun prototype-label (prototype)
  (if (null prototype) ""
      (concatenate 'string "\\<\\<" prototype "\\>\\>"))) 

;;拼接可能的原型
(defun class-name-label (name prototype)
  (if  (null prototype)  name
       (concatenate 'string (prototype-label prototype) "\\n" name)))


;;判断一个方法是否是抽象方法
(defun abstractp (method)
  (and (consp method) (member :abstract method)))

;;提高可读性的简单方法
(defun attributes (&rest attributes)
  attributes)
(defun methods (&rest methods)
  methods)

;;存放节点的库
(defparameter *node-table* (make-hash-table :test 'equal))
(defun reg-node (name node)
  (setf (gethash name *node-table*) node)
  node)
(defun @name (name)
  (gethash name *node-table*))

;;创建完整类
(defparameter *share-methods* nil)
(defun full-class (name  &optional prototype  attributes  methods)
  (reg-node name
            ([]  (concatenate 'string "{" (class-name-label name prototype)  "|"
                                          (multi-rows-label attributes)      "|"
                                          (multi-rows-label *share-methods*)
                                          (multi-rows-label methods)  "}"))))
;;共享方法工具宏
(defmacro with-method ((&rest methods) &body body)
  `(let ((*share-methods*
          (append *share-methods* ',methods)))
     (&& ,@body)))

;;创建简单类
(defun simp-class (name &optional prototype)
  (reg-node name
            (<> (class-name-label name prototype)
                :shape "box")))

;;创建角色
(defun actor(name)
  (reg-node name
            (<> name
                :image (dump-node-to-png node-stick-man)
                :labelloc "b"
                :shape "none")))

;;创建用例
(defun ucas(name)
  (reg-node name
            (<> name :shape "ellipse")))


;;创建包
(defun pack(name)
  (reg-node name
            (<> name :shape "tab")))

;;创建组件
(defun comp(name)
  (reg-node name
            (<> name :shape "component")))

;;创建接口
(defun port(name)
  (reg-node name
            (<> name :shape "circle" :size 0.1 :labelloc "b")))

;;被继承   
(defun -genby- (node1 node2)
  (-> node1 node2 :dir "back" :arrowtail "empty" :arrowsize 2.0))

;;被多个继承
(defun -genby-* (orgion-node &rest nodes)
  (with-edge (:dir "back" :arrowtail "empty" :arrowsize 2.0)
    (apply #'->> orgion-node nodes)))

;;被实现
(defun -relby- (node1 node2)
  (-> node1 node2 :style "dashed" :dir "back" :arrowtail "empty" :arrowsize 2.0))

;;被多个实现
(defun -relby-* (orgion-node &rest nodes)
  (with-edge (:style "dashed" :dir "back" :arrowtail "empty" :arrowsize 2.0)
    (apply #'->> orgion-node nodes)))

;;组合
(defun -com- (node1 node2)
  (-> node1 node2 :dir "back" :arrowtail "diamond"))

;;组合多个组合
(defun -com-* (orgion-node &rest nodes)
  (with-edge (:dir "back" :arrowtail "diamond")
    (apply #'->> orgion-node nodes)))

;;聚合
(defun -agg- (node1 node2)
  (-> node1 node2 :dir "back" :arrowtail "odiamond"))

;;组合多个聚合
(defun -agg-* (orgion-node &rest nodes)
  (with-edge (:dir "back" :arrowtail "odiamond")
    (apply #'->> orgion-node nodes)))

;;依赖
(defun -dep- (prototype node1 node2)
  (-> node1 node2 :label (prototype-label prototype) :style "dashed" :arrowhead "open"))


;;组合多个依赖
(defun -dep-* (prototype orgion-node &rest nodes)
  (let ((in-donuts::*with-edge-context*
         (append in-donuts::*with-edge-context*
                 (list :label (prototype-label prototype)))))   
    (with-edge (:style "dashed" :arrowhead "open")
      (apply #'->> orgion-node nodes))))

;;连接多个依赖
(defun -dep-- (prototype orgion-node &rest nodes)
  (let ((in-donuts::*with-edge-context*
         (append in-donuts::*with-edge-context*
                 (list :label (prototype-label prototype)))))   
    (with-edge (:style "dashed" :arrowhead "open")
      (apply #'--> orgion-node nodes))))

;;简单连接
(defun -lin- (node1 node2)
  (-> node1 node2 :dir "none"))




