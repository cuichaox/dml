;;;; example for sequnce diagram using
(in-package :cl-user)
(use-package :dml)

(uiop:chdir (asdf:system-relative-pathname :dml "demo"))

(dml-create-sequence "legend" ()
  (&prog "==>ObjectA.SyncMsg1()/ret1" 
         (&chain "==>ObjectA.SyncMsg2()/ret2"
                 "==>ObjectB.SyncMsg2.1()/ret2.1"
                 "-->!ActiveObjectC.AsyMesg2.2()")))
         
         
            
(dml-create-sequence "nested" ()
  (&chain "==>open"))
