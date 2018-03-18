
(uiop:define-package :dml.seq.engine
    (:mix #:cl-cairo2
          #:dml.seq.core
          #:dml.seq.grid
          #:cl)
    (:export    #:draw-message)
    (:documentation "doc"))

(in-package :dml.seq.engine)

(defparameter *context-grid* nil)

;;参数
(defconstant MIN-X-MARGIN 24.0)
(defconstant INNER-MARGIN 12.0)
(defconstant MIN-Y-MARGIN 48.0)

;;忽略告警使用的工具函数
(defun ignore-warning (condition)
  (declare (ignore condition))
  (muffle-warning))

(defmacro igw (&rest forms)
  `(handler-bind ((warning #'ignore-warning))
     ,@forms))


(defun get-call-v-index (msg)
 (+ 1 (position msg (all-call-messages *context-message*))))

(defun get-object-h-index (obj)
  (or (position obj *context-objects*)
      -1))
   
(defun get-object-v-index (obj)
  (let ((newby (new-message obj)))
    (if newby (get-call-v-index newby) 0)))

(defun call-to-right-p (call)
  (>= (get-object-h-index (to-object call))
      (get-object-h-index (from-object call))))

(defgeneric fit-to-grid (element))
(defgeneric draw-dml-element (element))
  
;;对象的大小
(defmethod fit-to-grid ((obj object))
  (let* ((name-ext (igw (get-text-extents (name obj))))
         (h-index (get-object-h-index obj))
         (new-by (new-message obj))
         (v-index (if new-by (get-call-v-index new-by) 0))
         (half-hspace (+ INNER-MARGIN MIN-X-MARGIN (/ (text-width name-ext) 2)))
         (half-vspace (+ INNER-MARGIN MIN-Y-MARGIN (/ (text-height name-ext) 2))))
    (fit-left *context-grid* h-index half-hspace)
    (fit-right *context-grid* h-index half-hspace)
    (fit-up *context-grid* v-index half-vspace)
    (fit-down *context-grid* v-index half-vspace)))

(defmethod fit-to-grid ((msg call-message))
  (let* ((label-ext (igw (get-text-extents (label msg))))
         (width (+ (* 2 MIN-X-MARGIN)
                   (text-width label-ext)))
         (height (+ (* 2 MIN-Y-MARGIN)
                    (text-height label-ext))))
    (if (or (null (from-object msg)))            
        (fit-left *context-grid*
                  (get-object-h-index (to-object msg))
                  width)
        (if (call-to-right-p msg)
            (fit-right *context-grid*
                       (get-object-h-index (from-object msg))
                       width)
            (fit-left *context-grid*
                      (get-object-h-index (from-object msg))
                      width)))
    (fit-up *context-grid*
            (get-call-v-index msg)
            height)))

                     
(defmethod fit-to-grid ((msg message))
  (loop
     for call in (all-call-messages msg)
     do (fit-to-grid call)))

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

;;以指定中心绘制文本
(defun draw-text-center-at (text x y &optional (box t)) 
  (let ((ext (igw (get-text-extents text))))
    (move-to (- x (+  (text-x-bearing ext) (/ (text-width ext) 2)))
             (- y (+  (text-y-bearing ext) (/ (text-height ext) 2))))             
    (show-text text)
    (when box (rectangle (- x INNER-MARGIN (/ (text-width ext) 2))
                         (- y INNER-MARGIN (/ (text-width ext) 2))
                         (+ (* 2 INNER-MARGIN) (text-width ext))
                         (+ (* 2 INNER-MARGIN) (text-height ext)))
          (stroke))))
;;绘制虚线                  
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

;;绘制箭头
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

(defmethod draw-dml-element ((obj object))
  (let* ((hline (get-object-h-index obj))
         (hx (get-x-by-index *context-grid* hline))
         (vline (get-object-v-index obj))
         (hy (get-y-by-index *context-grid* vline)))
    (progn (draw-text-center-at (name obj) hx hy))))         

(defmethod draw-dml-element ((msg call-message))
  (let* ((from-obj (from-object msg))
         (to-obj (to-object msg))
         (from-x (if (null from-obj)
                     (+  MIN-X-MARGIN (get-x-by-index *context-grid* (- (get-object-h-index from-obj) 1)))                        
                     (get-x-by-index *context-grid* (get-object-h-index from-obj))))
         (from-y (get-y-by-index *context-grid* (get-call-v-index msg)))         
         (to-x (get-x-by-index *context-grid* (get-object-h-index to-obj)))
         (to-y from-y))
    (progn (move-to from-x from-y)
           (line-to to-x to-y)
           (stroke)
           (draw-arraw-cap from-x from-y to-x to-y)
           (if (call-to-right-p msg)
               (draw-text-start-at (label msg)  from-x from-y)
               (draw-text-end-to (label msg) from-x from-y)))))

(defmethod draw-dml-element  ((msg self-call))
  (let* ((from-obj (from-object msg))
         (from-x (get-x-by-index *context-grid* (get-object-h-index from-obj)))
         (from-y (get-y-by-index *context-grid* (get-call-v-index msg)))
         (x1 (+ from-x (* 2 MIN-X-MARGIN)))  (y1 from-y)
         (x2 x1)  (y2 (+ y1 MIN-Y-MARGIN))
         (x3 from-x) (y3 (+ from-y 5)))
    (progn (move-to from-x from-y)
           (curve-to  x1 y1 x2 y2 x3 y3)
           (stroke)
           (draw-arraw-cap x2 y2 x3 y3)
           (draw-text-start-at (label msg) from-x from-y))))
                     

(defmethod draw-dml-element ((msg message))
  (loop
     for call in (all-call-messages msg)
     do (draw-dml-element call)))

(defparameter *context-message* nil)

(defun dock-all-to-grid()
  (progn
    (loop
       for obj in *context-objects*)
    (fit-to-grid *context-message*)))

(defparameter *contex* nil)

;;最后要实现的工具宏
(defun make-sequnce-diagram (outfile msg)
  (let* ((*context-message*  msg)
         (*context-grid*
          (make-instance
           'grid
           :hsize (length *context-objects*)
           :vsize (+ 1 (length (all-call-messages *context-message*))))))
     (setf *context* (create-ps-context (concatenate 'string
                                                    "/tmp/"
                                                    outfile
                                                    ".ps")
                                       600 600))
     (dock-all-to-grid)
     (loop
        for obj in *context-objects*
        do (draw-dml-element obj))
     (draw-dml-element msg)
     (destroy *context*)))
         

