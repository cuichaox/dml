
(uiop:define-package :dml.grid
    (:use :cl)
  (:export #:fit))



(defclass grid ()
  ((ranks :initform (make-array 0))))

