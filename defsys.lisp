(defsystem wam (:optimize ((speed 0) (space 0) (safety 3) (debug 3)))
  :members ("const"
            "wam"
            "wutil"
            "opcodes"
            "io"
            "asm"
            "parse"
            "alloc1"
            "coder"
            "test/parse-test"
            "test/test"
            )
  :rules ((:compile :all (:requires (:load :previous)))))

