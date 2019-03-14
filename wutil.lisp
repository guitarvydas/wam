; $Id$
                                        ; Copyright 2005 Paul Tarvydas
(in-package :wam/debug)

(defun dump ()
  (format t "p=~A cp=~A s=~A h=~A hb=~A b=~A b0=~A e=~A tr=~A mode=~A~%"
          ;; TODO: export these
          wam::p wam::cp wam::s wam::h wam::hb wam::b wam::b0 wam::e wam::tr wam::mode)
  (format t "regs:  ")
  (loop for i from 0 to 7 do
        (dump-cell i))
  (format t "~%")
  (format t "stack[~A]: " wam::stack-start) ;; TODO: export
  (loop for i from wam::stack-start to (+ wam::e 2 5) do ;; TODO: export X 2
        (dump-cell i))
  (format t "~%")
  (format t "heap:  ")
  (loop for i from wam::heap-start to (+ wam::heap-start 7) do ;; TODO: export
        (dump-cell i))
  (format t "~%"))

(defun dump-cell (i)
  (if (= (wam:regx i) 0)
      (format t "-- ")
    (format t "[~A ~A] "
            (dump-tag (wam:regx i))
            (if (= wam/tags:con (wam/tags:tag (wam:regx i)))
                (unconst (wam/tags:untag (wam:regx i)))
              (dump-untag (wam:regx i))))))

(defun unconst (x)
  (gethash x wam::unconsts)) ;; TODO: export

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
