(in-package :cl-user)

(defpackage :dml-seq
  (:use :cl :cl-cairo2 :defclass-std))

(in-package :dml-seq)

;;定义基本类型
(defclass/std object ()
  ((name)
   (type)
   (actp :std nil)))


(defcalss/std message ()
  ((name)
   (ret)
   ()))

;;

(defvar *object-table* (make-hash-table))
(defvar *message-set* nil)
(defvar *frame-list*)

;;


