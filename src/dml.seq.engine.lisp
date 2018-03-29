(uiop:define-package :dml.seq.engine
    (:mix #:cl-cairo2
          #:dml.seq.core
          #:dml.seq.grid
          #:cl)
  (:export   #:make-sequnce-diagram
             #:&prog
             #:&chain
             #:&if
             #:&loop
             #:*context-objects*
             #:*context-sequnce-attrs*)
  (:documentation "doc"))

(in-package :dml.seq.engine)

(defparameter *context-grid* nil)
(defparameter *context-message* nil)
(defparameter *context-sequnce-attrs*
  `(:font-size 20
    :line-with 1.0
    :background-rgba (1.0 1.0 1.0 1.0)
    :fore-normal-rgba (0.0 0.0 0.0 1.0)
    :fore-dim-rgba (0.5 0.5 0.6 1.0)))

;;参数
(defconstant +min-x-margin+ 8.0)
(defconstant +inner-margin+ 6.0)
(defconstant +min-y-margin+ 10.0)
(defconstant +half-bar-width+ 4.0)

;;忽略告警使用的工具函数
(defun ignore-warning (condition)
  (declare (ignore condition))
  (muffle-warning))

(defmacro igw (&rest forms)
  `(handler-bind ((warning #'ignore-warning))
     ,@forms))

(defun get-call-v-index (msg)
  (+ 1 (position msg (all-call-messages *context-message*))))

(defun get-caller-phases (obj)  
 (let ((ret nil))
   (reverse (dolist (caller (callers obj) ret)
              (push (cons (get-call-v-index (car caller))
                          (get-call-v-index (cdr caller)))
                    ret)))))

(defun compress-caller-phases (phases)
  (if (or (null phases) (null (cdr phases)))
      phases
      (if (< (cdar phases) (caadr phases))
          (cons (car phases) (compress-caller-phases (cdr phases)))
          (progn
            (setf (caadr phases) (caar phases))
            (compress-caller-phases (cdr phases))))))

(defun get-active-bars (obj)
  (if (is-active obj)
      (let ((begin-y (get-y-by-index *context-grid*
                                     (1+ (if (new-message obj)
                                             (get-call-v-index (new-message obj))
                                             0))))
            (end-y (if (end-message obj)
                       (get-call-v-index (end-message obj))
                       (- (get-height *context-grid*) +min-y-margin+))))
        (list (cons begin-y end-y)))
      (let ((ret nil))
        (reverse (dolist (bar (compress-caller-phases (get-caller-phases obj)) ret)
                   (push (cons (get-y-by-index *context-grid* (car bar))
                               (get-y-by-index *context-grid* (cdr bar)))
                         ret))))))

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

;;得到消息边界，的边距
(defgeneric up-extra-space (msg))
(defgeneric left-extra-space(msg))

(defmethod up-extra-space ((msg message)) 0)
(defmethod left-extra-space((msg message)) 0)

(defmethod up-extra-space ((msg call-message))
  (let* ((label-ext (igw (get-text-extents (label msg)))))
    (+ +min-y-margin+ (text-height label-ext))))

(defmethod up-extra-space ((msg guard-message))
  (let* ((guard-ext (igw (get-text-extents (guard msg)))))
    (+ (up-extra-space (the-message msg))
       +min-y-margin+
       (text-height guard-ext))))

(defmethod left-extra-space ((msg frame-guard))
  (let* ((label-ext (igw (get-text-extents (label msg)))))
    (+ (left-extra-space (the-message msg))
       +min-x-margin+
       +half-bar-width+
       +min-x-margin+
       (text-width label-ext))))


;;得到消息的边界,在网格系统中的位置)
(defgeneric left-side-index (msg))
(defgeneric right-side-index (msg))
(defgeneric up-side-index (msg))
(defgeneric down-side-index (msg))

(defmethod left-side-index ((msg call-message))
  (if (null (from-object msg))
      (get-object-h-index (to-object msg))
      (if (null (to-object msg))
          (get-object-h-index (from-object msg))
          (min (get-object-h-index (from-object msg))
               (get-object-h-index (to-object msg))))))

(defmethod right-side-index ((msg call-message))
  (if (null (from-object msg))
      (get-object-h-index (to-object msg))
      (if (null (to-object msg))
          (get-object-h-index (from-object msg))
          (max (get-object-h-index (from-object msg))
               (get-object-h-index (to-object msg))))))
  

(defmethod up-side-index ((msg call-message))
  (get-call-v-index msg))

(defmethod down-side-index ((msg call-message))
  (get-call-v-index msg))

(defmethod left-side-index ((msg message))
  (apply #'min (mapcar #'left-side-index (all-call-messages msg))))

(defmethod right-side-index ((msg message))
  (apply #'max (mapcar #'right-side-index (all-call-messages msg))))

(defmethod up-side-index ((msg message))
  (get-call-v-index (first (all-call-messages msg))))

(defmethod down-side-index ((msg message))
  (get-call-v-index (car (last (all-call-messages msg)))))

;;对象的大小
(defmethod fit-to-grid ((obj object))
  (let* ((name-ext (igw (get-text-extents (name obj))))
         (h-index (get-object-h-index obj))
         (new-by (new-message obj))
         (v-index (if new-by (get-call-v-index new-by) 0))
         (half-hspace (+ +inner-margin+ +min-x-margin+ (/ (text-width name-ext) 2)))
         (half-vspace (+ +inner-margin+ +min-y-margin+ (/ (text-height name-ext) 2))))
    (fit-left *context-grid* h-index half-hspace)
    (fit-right *context-grid* h-index half-hspace)
    (fit-up *context-grid* v-index half-vspace)
    (fit-down *context-grid* v-index half-vspace)))

(defmethod fit-to-grid ((msg call-message))
  (let* ((label-ext (igw (get-text-extents (label msg))))
         (width (+ (* 2 +inner-margin+)
                   (* 2 +min-x-margin+)
                   (text-width label-ext)))
         (height (+ (* 2 +min-y-margin+)
                    (text-height label-ext))))
    (if (null (from-object msg))            
        (fit-left *context-grid*
                  (get-object-h-index (to-object msg))
                  width)
        (if (null (to-object msg))
            (fit-left *context-grid*
                      (get-object-h-index (from-object msg))
                      width)  
            (if (call-to-right-p msg)
                (fit-right *context-grid*
                           (get-object-h-index (from-object msg))
                           width)
                (fit-left *context-grid*
                          (get-object-h-index (from-object msg))
                          width))))
    (fit-up *context-grid*
            (get-call-v-index msg)
            height)))
                     
(defmethod fit-to-grid ((msg message))
  (loop
     for call in (all-call-messages msg)
     do (fit-to-grid call)))

(defmethod fit-to-grid ((msg group-message))
  (dolist (m (messages msg))
    (fit-to-grid m)))

(defmethod fit-to-grid ((msg guard-message))
  (progn
    (fit-up *context-grid* (up-side-index msg) 
            (+ (up-extra-space msg)
               +min-y-margin+))
    (fit-right *context-grid* (left-side-index msg)
               (+ (* 2 +min-x-margin+)
                  (* 2 +half-bar-width+)
                  (text-width (igw (get-text-extents (guard msg))))))
    (fit-to-grid (the-message msg))))         

(defmethod fit-to-grid((msg frame-guard))
  (progn
    (fit-left *context-grid* (left-side-index msg)
              (+ (left-extra-space msg)
                 +min-x-margin+))
    (call-next-method)))

;;绘制边框用
(defun draw-frame(name fx fy tx ty)
  (let ((ext (igw (get-text-extents name)))
        (small-margin (/ +inner-margin+ 3)))
    (rectangle fx fy (- tx fx) (- ty fy))
    (stroke)
    (move-to (+ small-margin (- fx (text-x-bearing ext)))
             (+ small-margin (- fy (text-y-bearing ext))))
    (show-text name)
    (move-to   fx (+ fy (* 2 small-margin) (text-height ext)))
    (rel-line-to (+ (* 2 small-margin) (text-width ext))  0)
    (rel-line-to (* 2 small-margin) (* -1 (+ (* 2 small-margin)  (text-height ext))))
    (stroke)))


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
    (when box (rectangle (- x +inner-margin+ (/ (text-width ext) 2))
                         (- y +inner-margin+ (/ (text-height ext) 2))
                         (+ (* 2 +inner-margin+) (text-width ext))
                         (+ (* 2 +inner-margin+) (text-height ext)))
          (stroke))))
;;绘制虚线                  
(defun draw-dash-line (fx fy tx ty)
  (save)
  (set-dash 0.0 '(8.0 2.0 4.0 2.0))  
  (move-to fx fy)
  (line-to tx ty)
  (stroke)
  (restore))
  

(defun draw-life-cycle-line (x fy ey bars)
  (let ((cy fy))   
   (dolist (bar bars)
     (draw-dash-line x cy x (car bar))
     (rectangle (- x +half-bar-width+) (car bar)
                (* 2 +half-bar-width+) (- (cdr bar) (car bar)))
     (stroke)
     (setf cy (cdr bar)))
   (draw-dash-line x cy x ey)))   

;;绘制不同类型的箭头
(defun draw-arraw-cap-line-left (fx fy tx ty)
  (let ((width 12)
        (height 8))         
    (progn (save)
           (translate tx ty)
           (rotate (phase (complex (- tx fx)
                                   (- ty fy))))
           (set-dash 0.0 '())
           (move-to 0 0) (line-to (* -1 width) height)           
           (stroke)
           (restore))))

(defun draw-arraw-cap-line-right (fx fy tx ty)
  (let ((width 12)
        (height 8))         
    (progn (save)
           (translate tx ty)
           (rotate (phase (complex (- tx fx)
                                   (- ty fy))))
           (set-dash 0.0 '())
           (move-to 0 0) (line-to (* -1 width) (* -1 height))           
           (stroke)
           (restore))))

(defun draw-arraw-cap-line-upside (fx fy tx ty)
  (if (> fx tx)
      (draw-arraw-cap-line-left fx fy tx ty)
      (draw-arraw-cap-line-right fx fy tx ty)))
  


(defun draw-arraw-cap-line-full (fx fy tx ty)
  (let ((width 10)
        (height 5))         
    (progn (save)
           (translate tx ty)
           (rotate (phase (complex (- tx fx)
                                   (- ty fy))))
           (set-dash 0.0 '())
           (move-to 0 0) (line-to (* -1 width) height)
           (move-to 0 0) (line-to (* -1 width) (* -1 height))
           (stroke)
           (restore))))

(defun draw-arraw-cap-tri (fx fy tx ty)
  (let ((width 8)
        (height 4))         
    (progn (save)
           (translate tx ty)
           (rotate (phase (complex (- tx fx)
                                   (- ty fy))))           
           (move-to 0 0)
           (line-to (* -1 width) height)
           (line-to (* -1 width) (* -1 height))
           (close-path)
           (cl-cairo2:fill-path)
           (restore))))

;;根据调用类型选择绘制方法
(defun draw-arraw-cap-for-msg(msg fx fy tx ty)
  (cond ((or (typep msg 'syn-call) (typep msg 'ret-call)) (draw-arraw-cap-tri fx fy tx ty))
        ((typep msg 'asy-call) (draw-arraw-cap-line-upside fx fy tx ty))
        (t (draw-arraw-cap-line-full fx fy tx ty))))
          

(defmethod draw-dml-element ((obj object))
  (let* ((hline (get-object-h-index obj))
         (hx (get-x-by-index *context-grid* hline))
         (vline (get-object-v-index obj))
         (hy (get-y-by-index *context-grid* vline))
         (ext (igw (get-text-extents (name obj)))))
    (progn (draw-text-center-at (name obj) hx hy)
           (draw-life-cycle-line
            hx
            (+ hy +inner-margin+  (/ (text-height ext) 2))
            (- (get-height *context-grid*) +inner-margin+)
            (get-active-bars obj)))))

(defmethod draw-dml-element ((msg call-message))
  (let* ((from-obj (from-object msg))
         (to-obj (to-object msg))
         (from-x (if (null from-obj)
                     (+  +min-x-margin+ (get-x-by-index *context-grid* (- (get-object-h-index to-obj) 1)))                        
                     (get-x-by-index *context-grid* (get-object-h-index from-obj))))
         (from-y (get-y-by-index *context-grid* (get-call-v-index msg)))         
         (to-x (if (null to-obj)
                   (+  +min-x-margin+ (get-x-by-index *context-grid* (- (get-object-h-index from-obj) 1)))
                   (get-x-by-index *context-grid* (get-object-h-index to-obj))))
         (to-y from-y)
         (to-right (call-to-right-p msg))
         (fx-offset (if to-right +half-bar-width+ (* -1 +half-bar-width+)))
         (tx-offset (* -1 fx-offset)))
    (progn (incf from-x fx-offset)
           (incf to-x tx-offset)
           (move-to from-x from-y)
           (line-to to-x to-y)
           (stroke)
           (draw-arraw-cap-for-msg msg from-x from-y to-x to-y)
           (if to-right
               (draw-text-start-at (label msg)  (+ +inner-margin+ from-x) from-y)
               (draw-text-end-to (label msg) (- from-x +inner-margin+) from-y)))))

(defmethod draw-dml-element  ((msg self-call))
  (let* ((from-obj (from-object msg))
         (from-x (+ +half-bar-width+ (get-x-by-index *context-grid* (get-object-h-index from-obj))))
         (from-y (get-y-by-index *context-grid* (get-call-v-index msg)))
         (x1 (+ from-x (* 4 +min-x-margin+)))  (y1 from-y)
         (x2 x1)  (y2 (+ y1 (* 3 +min-y-margin+)))
         (x3 from-x) (y3 (+ from-y +min-y-margin+)))
    (progn (move-to from-x from-y)
           (curve-to  x1 y1 x2 y2 x3 y3)
           (stroke)
           (draw-arraw-cap-for-msg msg x2 y2 x3 y3)
           (draw-text-start-at (label msg) from-x from-y))))

(defmethod draw-dml-element ((msg ret-call))
  (save)
  (set-dash 0.0 '(8.0 2.0))
  (call-next-method)
  (restore))

(defmethod draw-dml-element ((msg group-message))
  (dolist (one-msg (messages msg))
    (draw-dml-element one-msg)))

(defmethod draw-dml-element ((msg message))
  (loop
     for call in (all-call-messages msg)
     do (draw-dml-element call)))

(defmethod draw-dml-element ((msg guard-message))
  (progn (draw-text-end-to (guard msg)
                           (- (get-x-by-index *context-grid* (1+ (left-side-index msg)))
                              +min-x-margin+)
                           (+ (- (get-y-by-index *context-grid* (up-side-index msg))
                                 (up-extra-space msg))
                              (text-height (igw (get-text-extents (guard msg))))))
         (draw-dml-element (the-message msg))))
                           

(defmethod draw-dml-element ((msg frame-guard))
  (let* ((up-space (up-extra-space msg))
         (left-space (left-extra-space msg))
         (fx (- (get-x-by-index *context-grid* (left-side-index msg))
                left-space))
         (tx (+ (get-x-by-index *context-grid* (right-side-index msg))
                +min-x-margin+))
         (fy (- (get-y-by-index *context-grid* (up-side-index msg))
                up-space))
         (ty (+ (get-y-by-index *context-grid* (down-side-index msg))
                +min-y-margin+)))    
    (draw-frame (label msg) fx fy tx ty)
    (call-next-method)))   

(defun dock-all-to-grid()
  (progn
    (loop
       for obj in *context-objects*
       do  (fit-to-grid obj))
    (fit-to-grid *context-message*)))


(defun make-sequnce-diagram (name msg)
  (let* ((*context-message*  msg)
         (grid-hsize (length *context-objects*))
         (grid-vsize (+ 1 (length (all-call-messages *context-message*))))
         (*context-grid* (make-instance 'grid :hsize grid-hsize :vsize grid-vsize))
         (ps-surface (create-ps-surface (concatenate 'string name ".ps") 200 200))
         (*context* (create-context ps-surface)))         
    (fit-down *context-grid* (- grid-vsize 1) +min-y-margin+)
    (set-font-size (getf *context-sequnce-attrs* :font-size))
    (dock-all-to-grid)
    (ps-surface-set-size ps-surface (get-width *context-grid*) (get-height *context-grid*))
    (apply #'set-source-rgba (getf *context-sequnce-attrs* :background-rgba))
    (paint)
    (apply #'set-source-rgba (getf *context-sequnce-attrs* :fore-normal-rgba))
    (set-line-width (getf *context-sequnce-attrs* :line-with))
    (loop
       for obj in *context-objects*
       do (draw-dml-element obj))    
    (draw-dml-element msg)
    (surface-write-to-png ps-surface (concatenate 'string name ".png"))    
    (destroy ps-surface)        
    (setf *context-objects* nil)
    (destroy *context*)))    

