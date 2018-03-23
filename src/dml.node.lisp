(defpackage   :dml.node
  (:use     #:cl #:cl-cairo2)
  (:export  #:draw-node
            #:dump-node-to-ps
            #:dump-node-to-png
            #:node-stick-man)
  (:documentation "define node symbol by path."))

(in-package :dml.node)

#|
   { (<draw-cmd> <path-cmd> *) }*

|#

(defun draw-node (node)
  (dolist (cmd (getf node :commands))
    (apply (car cmd) (cdr cmd))))

(defun tmp-ps(extname)
  (concatenate 'string
               (uiop:native-namestring (uiop:tmpize-pathname (uiop:temporary-directory)))
               extname))
  

(defun dump-node-to-ps (node &optional (to-scale 10) (fname nil))
  (let* ((width  (* to-scale (getf node :width)))
         (height (* to-scale (getf node :height)))
         (ps-file-name (or fname (tmp-ps)))
         (*context* (create-ps-context ps-file-name width height)))
    (scale to-scale to-scale)
    (set-line-width (/ 1 to-scale))
    (draw-node node)
    (destroy *context*)
    ps-file-name))


(defun dump-node-to-png (node &optional (to-scale 15) (fname nil))
  (let* ((width  (* to-scale (getf node :width)))
         (height (* to-scale (getf node :height)))
         (png-file-name (or fname (tmp-ps ".png"))))
    (with-png-file (png-file-name :rgb24 width height)
      (set-source-rgb 1 1 1)
      (paint)
      (scale to-scale to-scale)
      (set-source-rgb 0 0 0)
      (set-line-width (/ 2 to-scale))
      (draw-node node))      
    png-file-name))



(defparameter node-stick-man
  `(:width 4 :height 10
    :commands       
    ((arc 2 1 1 0 ,(* 2 pi))
     (move-to 2 2) (line-to 2 6)
     (move-to 0 4) (line-to 4 4)
     (move-to 0 10) (line-to 2 6) (line-to 4 10)     
     (stroke))))

