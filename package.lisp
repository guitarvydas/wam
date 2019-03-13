(defpackage wam
  (:use :cl)
  (:export
   #:code
   #:defrel0
   #:defrel
   #:defquery
   #:init-opcodes
   #:reset-code
   #:reset-asm
   #:?-
   #:regx
   #:parse-query
   #:parse-rule))

(defpackage wam/debug
  (:use :cl
        :wam)
  (:export
   #:tprint
   #:dump))

(defpackage wam/tags
  (:use :cl
        :wam)
  (:export
   #:untag
   #:funtag
   #:tag
   #:int
   #:ref
   #:con
   #:spcl
   #:lis
   #:str
   #:tag-int
   #:tag-ref
   #:tag-con
   #:tag-spcl
   #:tag-lis
   #:tag-str))

(defpackage wam/test
  (:use :cl
        :wam))

