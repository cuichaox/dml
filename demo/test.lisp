;;准备好绘制环境

(defparameter *context*
  (create-ps-context
   (concatenate 'string
                (uiop:native-namestring
                 (uiop:temporary-directory))
                "Temp.DML.ps")
   200 200))



;;测试绘制函数
(with-png-file ("/tmp/test.png" :rgb24 400 200)
  (set-source-rgb 0.2 0.2 1)
  (paint)
  (set-font-size 100)
  (set-source-rgb 1 1 1)
  (draw-text-center-at "hello" 0 0)  
  (line-to 60 90)  
  (set-line-width 5)
  (stroke))
