
(define-package :dml.grid
    (:use :cl)
  (:export #:grid
           #:fit-left           
           #:fit-right
           #:fit-up
           #:fit-down
           #:get-x-by-index
           #:get-y-by-index))

(defclass grid ()
  ((h-lines :initform nil)
   (v-lines :initform nil)))

(defmethod initialize-instance ((g grid) &key hsize vsize)
  (with-slots (h-lines v-lines) g
    (setf h-lines (make-array hsize))
    (dotimes (i hsize) (setf (aref h-lines i) (cons 0 0))) 
    (setf v-lines (make-array vsize))
    (dotimes (i vsize) (setf (aref v-lines i) (cons 0 0)))))

(defmacro def-fit (name  acc-name slot-name)
  `(defun ,name (grid index space)
     (with-slots (,slot-name) grid
       (symbol-macrolet ((the-space (,acc-name (aref ,slot-name index))))
         (when (> space the-space) (setf the-space space))))))

(def-fit fit-left car v-lines)
(def-fit fit-right cdr v-lines)
(def-fit fit-up car h-lines)
(def-fit fit-down cdr h-lines)

(defun get-all-space (lines index)
  (reduce #'+ lines
          :end index
          :key #'(lambda (line) (+ (car line) (cdr line)))
          :initial-value (car (elt lines index))))

(defun get-x-by-index (grid v-index)
  (with-slots (v-lines) grid
    (get-all-space v-lines v-index)))

(defun get-y-by-index ( grid h-index)
  (with-slots (h-lines) grid
    (get-all-space h-lines h-index)))



