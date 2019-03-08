(defsystem wam
  :components ((:file "package")))

(defsystem wam/all
  :depends-on (wam)
  :components ((:module source :pathname "./" :components
                        ((:file "wutil") ;; FIXME:  DUMP-TAG doesn't work on type selector
                         (:file "coder")
                         (:file "asm")
                         (:file "const")
                         (:file "parse")
                         (:file "io")
                         (:file "opcodes")
                         (:file "alloc1")
                         (:file "wam")))))

(defsystem wam/test
  :depends-on (wam)
  :components ((:module suite :pathname "test/" :components
                        ((:file "parse-test")
                         (:file "test")))))

(defsystem wam/compare
  :depends-on (wam)
  :components ((:module suite :pathname "compare/" :components
                        ((:static-file "grandfather.pl")  ;; TODO add proper class for Prolog source files
                         (:static-file "t13.pl")
                         (:static-file "t14.pl")
                         (:static-file "t15.pl")))))


                        


               
