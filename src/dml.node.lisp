(defpackage   :dml.node
  (:use       :cl :cl-cairo2)
  (:export    #:dml-path)
  (:documentation "define node symbol by path."))

(in-package :dml.node)


(defparameter +cario-acts+ `((:>> #'move-to))
                            (:-> #'line-to)
                            (:~> #'curve-to)
                            (:<< #'close))

(defparameter +stick-man+ '((:>> 2 0) (:~> 3 0 3 2 2 2) (:~> 1 2 1 1 2 0)) ;head
                           (:>> 2 2) (:-> 8 2)                           ;body
                           (:>> 0 4) (:-> 4 4)                         ;arms
                           (:>> 0 10) (:-> 2 6) (-> 4 10))            ;legs

(defun get-act-fun (director)
  (or (cdr (assoc director +cario-acts+))      
      (error (format nil  "Unkonw path director: ~s" director))))

(defun dml-path (dpt)
  (dolist (dir dpt)
    (apply (get-act-fun (car dir))
           (cdr dpt))))


