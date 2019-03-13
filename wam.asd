(defsystem wam
  :components ((:file "package")
               (:file "store")))

(defsystem wam/tags
  :depends-on (wam)
  :components ((:module source :pathname "./" :components
                ((:file "tags")))))

(defsystem wam/debug
  :depends-on (wam wam/tags)
  :components ((:module source :pathname "./" :components
                        ((:file "wam-debug")
                         (:file "wutil")))))

(defsystem wam/all
  :depends-on (wam wam/debug wam/tags)
  :components ((:module source :pathname "./" :components
                        ((:file "const")
                         (:file "opcodes")
                         (:file "io")
                         (:file "parse")
                         (:file "asm")
                         (:file "alloc1")
                         (:file "coder")
                         (:file "wam")))))

(defsystem wam/test
  :depends-on (wam wam/debug)
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
