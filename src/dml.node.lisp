(defpackage   :dml.node
  (:use       :cl :cl-cairo2)
  (:export    #:dml-path)
  (:documentation "define node symbol by path."))

(in-package :dml.node)


(defparameter +cario-acts+ '((:>> . move-to) (:-> . line-to) (:~> . curve-to) (:<< . close)))

(defparameter +stick-man+ '((:>> 2 0)  (:~> 2.447 0 3 0.553 3 1)
                            (:~> 3 1.447 2 1.553 2 2)
                            (:~> 1.553 2 0.553 1 1 1)
                            (:~> 1 0.447 1.553 0 2 0)
                            (:>> 2 2) (:-> 2 6)                           ;body
                            (:>> 0 4) (:-> 4 4)                         ;arms
                            (:>> 0 10) (:-> 2 6) (:-> 4 10)))            ;legs

(defun get-act-fun (director)
  (or (cdr (assoc director +cario-acts+))      
      (error (format nil  "Unkonw path director: ~s" director))))

(defun dml-path (dpt)
  (dolist (dir dpt)
    (apply (get-act-fun (car dir))
           (cdr dir))))


