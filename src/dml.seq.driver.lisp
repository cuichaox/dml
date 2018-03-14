
(in-package :dml.seq)

;;参数
(defconstant MIN-X-MARGIN 12.0)
(defconstant MIN-Y-MARGIN 24.0)

;;
(defun ignore-warning (condition)
  (declare (ignore condition))
  (muffle-warning))
(defmacro igw (&rest forms)
  `(handler-bind ((warning #'ignore-warning))
     ,@forms))

;;不变量形式存在的对象范围
(defclass dml-extents ()
  ((x-bearing :reader x-bearing :initarg :x-bearing :initform 0.0)
   (y-bearing :reader y-bearing :initarg :y-bearing :initform 0.0)
   (label-end :reader width :initarg :width :initform 0.0)
   (height :reader height :initarg :height :initform 0.0)))

(defgeneric get-dml-extents (element)
  (:documentation "get dml extents in current contex"))

(defgeneric draw-dml-element (element x y)
  (:documentation "Draw the given objectect in current contex."))

;;从调用消息起点开始绘制消息
(defmethod get-dml-extents ((msg call-message))
  (let ((label-ext (igw (get-text-extents (label call-message)))))
    (make-instance 'dml-extents :x-bearing 0.0
                                :y-bearing (* -1 MIN-Y-MARGIN)
                                :width (+ (* 2 MIN-X-MARGIN) (text-width label-ext))
                                :height (+ (* 2 MIN-Y-MARGIN) (text-height label-ext)))))
;;以左下角为起点绘制文本
(defun draw-text-on-start-point(text x y)
  (let (ext (igw (get-text-extents text)))
    (move-to (- x (text-x-bearing ext))
             (- y (text-y-bearing ext)))
    (show-text text)))

;;绘制直线箭头
(defun draw-arrow from-x from-y to-x to-y :style)


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

;;准备好绘制环境

(defparameter *context*
  (create-ps-context
   (concatenate 'string
                (uiop:native-namestring
                 (uiop:temporary-directory))
                "Temp.DML.ps")
   200 200))

;;测试绘制函数
(with-png-file ("/tmp/test.png" :rgb24 200 200)
  (set-source-rgb 0.2 0.2 1)
  (paint)
  (translate 100 100)
  (rotate 1.15)
  (move-to 0 0)
  (line-to 60 0)
  (set-source-rgb 1 1 1)
  (set-line-width 5)
  (stroke))
