; $Id$
                                        ; Copyright 2005 Paul Tarvydas
(in-package :wam/debug)

(defun dump ()
  (format t "p=~A cp=~A s=~A h=~A hb=~A b=~A b0=~A e=~A tr=~A mode=~A~%"
          p cp s h hb b b0 e tr mode)
  (format t "regs:  ")
  (loop for i from 0 to 7 do
        (dump-cell i))
  (format t "~%")
  (format t "stack[~A]: " stack-start)
  (loop for i from stack-start to (+ e 2 5) do
        (dump-cell i))
  (format t "~%")
  (format t "heap:  ")
  (loop for i from heap-start to (+ heap-start 7) do
        (dump-cell i))
  (format t "~%"))

(defun dump-cell (i)
  (if (= (wam:regx i) 0)
      (format t "-- ")
    (format t "[~A ~A] "
            (dump-tag (wam:regx i))
            (if (= con (wam/tags:tag (wam:regx i)))
                (unconst (wam/tags:untag (wam:regx i)))
              (dump-untag (wam:regx i))))))

(defun unconst (x)
  (gethash x unconsts))

(defun dump-tag (x)
  (case (wam/tags:tag x)
    (#.wam/tags:int "int")
    (#.wam/tags:ref "ref")
    (#.wam/tags:con "con")
    (#.wam/tags:lis "lis")
    (#.wam/tags:str "str")
    (#.wam/tags:spcl "spcl")
    (otherwise "unknown")))

(defun dump-untag (x)
  (wam/tags:untag x))
