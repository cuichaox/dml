;;;; example for sequnce diagram using
(in-package :cl-user)
(use-package :dml)

(uiop:chdir (asdf:system-relative-pathname :dml "demo"))

(dml-create-sequnce "legend" ()
  (&go "==>ObjectA.SyncMsg1()/ret1" 
       (&in "==>ObjectA.SyncMsg2()/ret2"
            "==>ObjectB.SyncMsg2.1()/ret2.1")))          
            
   
