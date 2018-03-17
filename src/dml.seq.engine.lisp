
(uiop:define-package :dml.seq.engine
    (:mix #:cl-cairo2
          #:dml.seq.core
          #:cl)
    (:export    #:draw-message)
    (:documentation "doc"))

(in-package :dml.seq.engine)

;;参数
(defconstant MIN-X-MARGIN 12.0)
(defconstant MIN-Y-MARGI t [doc])value [doc]N 24.0

;;忽略告警使用的工具函数
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


(defgeneric fit (element)
  (:documentation "get dml extents in current contex"))

(defgeneric draw-dml-element (element x y)
  (:documentation "Draw the given objectect in current contex."))

;;对象的大小
(defmethod get-dml-extents ((msg call-message))
  (let ((label-ext (igw (get-text-extents (label msg)))))
    (make-instance 'dml-extents :x-bearing (if (call-to-right-p msg) 0.0
                                               (* -1 (+ (* 2 MIN-X-MARGIN)
                                                        (text-width label-ext))))
                                :y-bearing (* -1 MIN-Y-MARGIN)
                                :width (+ (* 2 MIN-X-MARGIN) (text-width label-ext))
                                :height (+ (* 2 MIN-Y-MARGIN) (text-height label-ext)))))


;;以制定坐标为起点绘制文本
(defun draw-text-start-at (text x y)
  (let ((ext (igw (get-text-extents text))))
    (move-to (- x (text-x-bearing ext))
             (- y (+ (text-y-bearing ext) (text-height ext))))
    (show-text text)))

;;以制定坐标为终点绘制文本
(defun draw-text-end-to (text x y)
  (let ((ext (igw (get-text-extents text))))
    (move-to (- x (+ (text-x-bearing ext) (text-width ext)))
             (- y (+ (text-y-bearing ext) (text-height ext))))             
    (show-text text)))

;;以制定左边为中心绘制文本
(defun draw-text-center-at (text x y) 
  (let ((ext (igw (get-text-extents text))))
    (move-to (- x (+  (text-x-bearing ext) (/ (text-width ext) 2)))
             (- y (+  (text-y-bearing ext) (/ (text-height ext) 2))))             
    (show-text text)))

;; look complex as point
(defun draw-dash-line (fx fy tx ty)
  (let* ((fp (complex fx fy))
         (tp (complex tx ty))
         (total-step  (- tp fp))
         (total-len (abs total-step))
         (dir (/ total-step total-len))
         (step-len 8.0))    
    (loop
       for cur-len from 0.0 to total-len by step-len       
       for cur-pos = (+ fp (* cur-len dir))
       for next-pos = (+ cur-pos (* 4.0 dir))
       do (print (list cur-pos next-pos))
       do (move-to (realpart cur-pos)
                   (imagpart cur-pos))       
       do (line-to (realpart next-pos) (imagpart next-pos)))
    (stroke)))

(defun draw-arraw-cap (fx fy tx ty &key (left t) (right t))
  (let ((w 20) (h 5))
    (progn (save)
           (translate tx ty)
           (rotate (phase (complex (- tx fx)
                                   (- ty fy))))
           (when left (move-to 0 0) (line-to (* -1 w) h))
           (when right (move-to 0 0) (line-to (* -1 w) (* -1 h)))
           (stroke)
           (restore))))

(defmethod draw-dml-element ((msg call-message) x y)
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


;;最后要实现的函数
(defun make-sequnce-dia (outfile msg)
  (let ()))

