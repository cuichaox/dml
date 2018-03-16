
(uiop:define-package :dml.grid
    (:use :cl)
  (:export #:fit))

(defclass grid ()
  ((h-lines :initform nil)
   (v-lines :initform nil)))

(defmethod initialize-instance ((g grid) &key hsize vsize)
  (with-slots (h-lines v-lines) g
    (setf h-lines (make-array hsize :initial-element (cons 0 0)))
    (setf v-lines (make-array vsize :initial-element (cons 0 0)))))

(defmacro def-fit (name  acc-name slot-name)
  `(progn (defgeneric ,'name (grid index space))
          (defmethod ,'name ((g grid) index space)
            (with-slots (,'slot-name) g
              (symbol-macrolet ((the-space (,'acc-name ,'slot-name)))
                (when (> space the-space) (setf the-space space)))))))


(def-fit fit-left car v-lines)




(defgeneric fit-left (grid v-line-index space)
  (:documentation "make sure left space of vline."))
(defmethod fit-left ((g grid) v-line-index space)
  (with-slots (v-lines) g
    (symbol-macrolet ((lp (car (aref v-lines v-line-index))))
       (when (> space lp) (setf lp space)))))
 
(defgeneric fit-right (grid v-line-index space)
  (:documentation "make sure right space of vline."))
(defmethod fit-right ((g grid) v-line-index space)
  (with-slots (v-lines) g
    (symbol-macrolet ((lp (cdr (aref v-lines v-line-index))))
       (when (> space lp) (setf lp space)))))

(defgeneric fit-up (grid h-line-index space)
  (:documentation "make sure up space of vline."))
(defmethod fit-up ((g grid) h-line-index space)
  (with-slots (h-lines) g
    (symbol-macrolet ((lp (car (aref h-lines h-line-index))))
       (when (> space lp) (setf lp space)))))


(defgeneric fit-down (grid h-line-index space)
  (:documentation "make sure down space of vline."))

(defmethod fit-up ((g grid) h-line-index space)
  (with-slots (h-lines) g
    (symbol-macrolet ((lp (cdr (aref h-lines h-line-index))))
       (when (> space lp) (setf lp space)))))

(defgeneric get-x-by-hi (args)
   (:documentation "doc"))

