;;;; example for sequnce diagram using
(asdf:load-system :dml)
(in-package :dml)

(uiop:chdir (asdf:system-relative-pathname :dml "demo"))

(dml-create-sequence "legend" ()
  (&prog "==>ObjectA.SyncMsg1()/ret1"
         (&chain "==>ObjectA.SyncMsg2()/ret2"
                 "==>ObjectB.SyncMsg2.1()/ret2.1"
                 "-->!ActiveObjectC.AsyMesg2.2()")))

(dml-create-sequence "nested" ()
  (&chain "==>aComputer.play()/"
          (&prog (&chain "==>emacs.run()/"
                         "==>slime.load()/")
                 (&chain "==>slime.Hacking()/"
                         (&chain "==>slime.LookupHyperSpec()"
                                 (&prog "==>chrome:Explorer.Start()/"
                                        "==>chrome:Explorer.OpenSpec()/"))))))

(dml-create-sequence "control" ()
  (&chain "==>:PersonFile.LoadAllGoodMan()/"
          (&loop "[hasNext]"
                 (&prog "==>:PersonFile.readNext()/"
                        (&if "[Is Good Man]"
                            (&prog "n=>aManObj.new"
                                   "==>manPool.register()/"))))))


