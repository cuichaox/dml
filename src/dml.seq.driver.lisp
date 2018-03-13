
(in-package :dml.seq)

;;不变量形式存在的对象范围
(defclass dml-extents ()
  ((x-bearing :reader x-bearing :initarg :x-bearing :initform 0)
   (y-bearing :reader y-bearing :initarg :y-bearing :initform 0)
   (label-end :reader width :initarg :width :initform 0)
   (height :reader height :initarg :width :initform 0)))

(defgeneric get-dml-extents(element)
  (:documentation "get dml extents in current contex"))

(defgeneric draw-dml-element (element x y)
  (:documentation "Draw the given objectect in current contex."))

;;从调用消息起点开始绘制消息
(defmethod get-dml-extents ((msg call-message))
  ())
(defmethod draw-dml-element ((msg call-message) x y)
  ())

;;为特定消息定制绘制
(defmethod get-dml-extents ((msg call-message))
  ())
(defmethod draw-dml-element  ((msg self-call) x y)
  ())

;;为特定消息类型指定绘制属性
(defmethod draw-dml-element :around ((msg syn-call) x y)
  ())
(defmethod draw-dml-element :around ((msg asy-call) x y)
  ())
(defmethod draw-dml-element :around ((msg ret-call) x y)
  ())
(defmethod draw-dml-element :around ((msg new-call) x y)
  ())
(defmethod draw-dml-element :around ((msg del-call) x y)
  ())

;;为特定消息补充绘制
(defmethod draw-dml-element :after ((msg del-call) x y)
  ())

;;从第一个消息的起点绘制消息组
(defmethod get-dml-extents ((msg group-message))
  ())
(defmethod draw-dml-element ((msg group-message) x y)
  ())

;;绘制guard和消息
(defmethod get-dml-extents ((msg guard-message))
  ())
(defmethod draw-dml-element ((msg guard-message) x y)
  ())

;;给分支循环绘制消息边框 应该用继承而不是组合

(defmethod get-dml-extents ((msg frame-message))
  ())

(defmethod draw-dml-element  ((msg frame-message) x y)
  ())

;;基本汇总函数集合

;; extend组合规则

