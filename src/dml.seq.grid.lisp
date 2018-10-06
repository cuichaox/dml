
(defpackage :dml.seq.grid
  (:use :cl)
  (:export #:grid
           #:fit-left
           #:fit-right
           #:fit-up
           #:fit-down
           #:get-width
           #:get-height
           #:get-x-by-index
           #:get-y-by-index))

(in-package :dml.seq.grid)

(defconstant +min-x-gap+ 8.0)
(defconstant +min-y-gap+ 8.0)
(defconstant +inner-margin+ 8.0)


(defclass grid ()
  ((h-lines :initform nil)
   (v-lines :initform nil)))

(defmethod initialize-instance ((g grid) &key hsize vsize)
  (with-slots (h-lines v-lines) g
    (setf h-lines (make-array hsize))
    (dotimes (i hsize) (setf (aref h-lines i) (cons 0 +min-x-gap+)))
    (setf v-lines (make-array vsize))
    (dotimes (i vsize) (setf (aref v-lines i) (cons 0 +min-y-gap+)))))


(defmacro def-fit (name  acc-name slot-name)
  `(defun ,name (grid index space)
     (with-slots (,slot-name) grid
       (symbol-macrolet ((the-space (,acc-name (aref ,slot-name index))))
         (when (> space the-space) (setf the-space space))))))

(def-fit fit-left car h-lines)
(def-fit fit-right cdr h-lines)
(def-fit fit-up car v-lines)
(def-fit fit-down cdr v-lines)

(defun get-all-space-to (lines index)
  (reduce #'+ lines
          :end index
          :key #'(lambda (line) (+ (car line) (cdr line)))
          :initial-value (+ +inner-margin+ (car (elt lines index)))))

(defun get-total-space (lines)
  (reduce #'+ lines
          :key #'(lambda (line) (+ (car line) (cdr line)))
          :initial-value (* 2 +inner-margin+)))

(defun get-width (grid)
  (with-slots (h-lines) grid
    (get-total-space h-lines)))

(defun get-height (grid)
  (with-slots (v-lines) grid
    (get-total-space v-lines)))

(defun get-y-by-index (grid v-index)
  (with-slots (v-lines) grid
    (get-all-space-to v-lines v-index)))

(defun get-x-by-index ( grid h-index)
  (if (< h-index 0)
      +inner-margin+
      (with-slots (h-lines) grid
        (get-all-space-to h-lines h-index))))



