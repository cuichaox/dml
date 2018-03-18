;;准备好绘制环境

(use-package :uiop)

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
  (set-line-width 5)
  (dash-line 0 0 200 200)
  (stroke)

 ;; test in repl
 (make-sequnce-diagram "test" (&in "==>obj1.msg1/ok" "==>obj2.msg2/ok" "==>obj3.msg3/ok")))
