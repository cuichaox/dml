(in-package :cl-user)

(defpackage :dml-seq
  (:use :cl ))

(in-package :dml-seq)


;; 常量
(defconstant MAX-Y 999)

;; 参数

;; 上下文
(defparameter *current-xpos* 0)
(defparameter *current-ypos* 0)
(defparameter *current-message* nil)

;; 绘制
(defgeneric draw (any) (:documentation "Draw the given object in current contex."))

;; 对象
(defclass obj.act ()
  ((up :accessor up :initarg up :initform 0)
   (down :accessor down :initarg down :initform MAX-Y))
  (:documentation "Active strip on  life cycle Line."))
(defclass obj()
  ((name :accessor name :initarg name :initform "")
   (acts :accessor acts :initarg acts :initform nil)
   (xpos :accessor xpos :initarg xpos :initform 0)
   (begin-ypos :accessor begin-ypos :initarg begin-ypos :initform 0)
   (end-ypos :accessor end-ypos :initform MAX-Y))
  (:documentation "Object on cylinders."))


;; 消息
(defclass msg ()
  ((label :accessor label :initarg label :initform "")
   (begin-ypos :accessor begin-ypos :initarg begin-ypos :initform *current-ypos*))
  (:documentation "message or message group"))

(defmethod initialize-instance :after ((m msg) &key)
  (incf *current-ypos*))

;; 消息范围
(defgeneric up (msg) (:documentation "Get upper ypos in the grid."))
(defgeneric down (msg) (:documentation "Get bottom ypos in the grid."))
(defgeneric left (msg) (:documentation "Get left xpos in the grid."))
(defgeneric right (msg) (:documentation "Get right x pos in the grid."))

;; 所有消息有唯一确定的上方
(defmethod up ((msg msg))
  (begin-ypos msg))

;; 调用消息
(defclass call-other (msg)
  ((from-xpos :accessor from-xpos :initarg from-xpos :initform *current-xpos*)
   (to-xpos :accessor to-xpos :initarg to-xpos :initform nil))
  (:documentation "Call to other."))

;; 调用消息只站
(defmethod down ((msg call-other))
  (up msg))

;; 调用消息有唯一的左边和唯一的右边
(defmethod left ((msg call-other))
  (min (from-xpos msg) (to-xpos msg)))
(defmethod right ((msg call-other))
  (max (from-xpos msg) (to-xpos msg)))



;; 同步调用
(defclass syn-call (call-other) ())

;; 异步调用
(defclass asy-call (call-other) ())

;; 返回消息
(defclass ret-call (call-other) ())

;; 创建消息
(defclass new-call (call-other) ())

;; 销毁消息
(defclass del-call (call-other) ())

;; 自调用
(defclass call-self(msg)
  ((the-xpos :accessor the-xpos :initarg the-xpos :initform *current-xpos*))
  (:documentation "call self."))

(defmethod initialize-instance :after ((m call-self) &key)
  (incf *current-ypos*))


(defmethod down ((msg call-self))
   (1+ (up msg)))

;; 自调用的左侧和右侧相等
(defmethod left ((msg call-self))
  (the-xpos msg))
(defmethod right ((msg call-self))
  (the-xpos msg))

;; 单纯的消息组
(defclass group-msg (multi-msg)
  ((msgs :accessor msgs :initarg msgs :initform nil)))

;; 包含多个消息的消息 
(defclass mutil-msg (msg) ())
(defgeneric all-msgs(mutil-msg))

;; 遍历所有子消息，得道范围
(defmethod up ((mg group-msg))
  (apply #'min
         (mapcar #'up (all-msgs mg))))
(defmethod down ((mg group-msg))
  (apply #'max
         (mapcar #'down (all-msgs mg))))
(defmethod left ((mg group-msg))
  (apply #'min
         (mapcar #'left (all-msgs mg))))
(defmethod right ((mg group-msg))
  (apply #'max
         (mapcar #'right (all-msgs mg))))


;; 带guard的消息组
(defclass guard-group (group-msg)
  ((guard :accessor guard :initarg guard :initform "")))

;; 可选
(defclass opt-groups (guard-msg)
  ((name :initform "opt")))

;; 循环
(defclass opt-groups (guard-msg)
  ((name :initform "loop")))

;; 二选一
(defclass alt-groups (multi-msg)
  ((name :initform "alt")
   (ifgroup :accessor ifgroup :initarg ifgroup :initform nil)
   (elsegroup :accessor elsegroup :initarg elsegroup :initform nil)))

;; 返回所有子消息
(defmethod all-msgs ((m alt-groups))
  (append (ifgroup m) (elsegroup m)))

;;构建对象
(defparameter *current-objects* nil)

;; 找到旧对象
(defun find-object-by-name (n)
  (find-if #'(lambda (obj)
               (string= n (name obj)))
           *current-objects*))

;; 添加新对象
(defun intern-object (name &key (act nil) (new nil))
  (or (find-object-by-name name)
      (let ((obj
             (make-instance 'obj
                            :act (char=  #\! (char name 0)) 
                            :name name
                            :xpos (1+ (length *current-objects*))
                            :begin-yos (if new *current-xpos* 0)))) 
        (if act (setf (acts obj) (list (make-instance 'obj.act))))
        (push obj *current-objects*)
        obj)))
;; 根据符号得到类名称
(defun msg-symbol (ch)
  (cdr (assoc ch '((#\= 'syn-call)
                   (#\- 'asy-call)
                   (#\n 'new-call)
                   (#\d 'del-call)
                   (#\/ 'ret-call)))))
;; Parse to ( <class-name>, <object-name>, <message-name>, <return-name> )


;; 制作一个消息       
(defun make-by-director (director-char)
  (progn (when (symbolp director)
           (setf director (symbol-name director)))
         
         (make-instance (msg-symbol (elt director 0))
                        :name name
                        :to-xpos to-x)))

;; 消息定义专用语言
(defun m-progn (&rest any))

