
(in-package :donuts)

;;把列表转换成多行label
(defun multi-rows-label (labels)
  (let ((ret ""))
    (dolist (row labels)
      (setf ret (concatenate 'string ret (if (consp row) (car row) row) "\\l")))
    ret))

;;拼接可能的原型
(defun class-name-label (name prototype)
  (if  (null prototype)
    name
    (concatenate 'string
     "\\<\\<"
     prototype
     "\\>\\>\\n"
     name)))


;;判断一个方法是否是抽象方法
(defun abstractp (method)
  (and (consp method) (member :abstract method)))

;;提高可读性的简单方法
(defun attributes (&rest attributes)
  attributes)
(defun methods (&rest methods)
  methods)

;;存放节点的库
(defvar *node-table* (make-hash-table))
(defun reg-node (name node)
  (setf (gethash name *node-table*) node)
  node)
(defun get-node (name)
  (gethash name *node-table*))

;;创建完整类
(defparameter *share-methods* nil)
(defun full-class (name  &optional prototype  attributes  methods)
  (reg-node name
            ([]  (concatenate 'string "{" (class-name-label name prototype)  "|"
                                          (multi-rows-label attributes)      "|"
                                          (multi-rows-label *share-methods*)
                                          (multi-rows-label methods)  "}"))))
;;创建简单类
(defun simp-class (name &optional prototype)
  (reg-node name
            (<> (class-name-label name prototype)
                :shape box)))   
;;被继承
(defun -genby- (node1 node2)
  (-> node1 node2 :dir "back" :arrowtail "empty"))

;;被多个继承
(defun -genby-*(orgion-node &rest nodes)
  (with-edge :dir "back" :arrowtail "empty"
       (->> orgion-node nodes)))
;;组合
(defun -com- (node1 node2)
  (-> node1 node2 :dir "back" :arrowtail ))


  





