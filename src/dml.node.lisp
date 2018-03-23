(defpackage   :dml.node
  (:use       :cl :cl-cairo2)
  (:export    #:dml-path)
  (:documentation "define node symbol by path."))

(in-package :dml.node)

#|
   { (<draw-cmd> <path-cmd> *) }*

|#

(defparameter +stick-man+
  `(:width 5 :height 10
    :commonds       
    ((arc 2 1 1 0 ,(* 2 pi))
     (move-to 2 2) (line-to 2 6)
     (move-to 0 4) (line-to 4 4)
     (move-to 0 10) (line-to 2 6) (line-to 4 10)
     (stroke))))


(defun dml-path (dpt)
  (dolist (dir dpt)
    (apply (get-act-fun (car dir))
           (cdr dir))))

(defun dump-to-ps (dpt &optional (scale 10)))




