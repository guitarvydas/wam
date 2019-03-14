;;; Copyright 2005 Paul Tarvydas
(in-package :wam/test)

(defun labelp (x)
  (char= #\$ (char (symbol-name x) 0)))

(defun refp (x)
  (char= #\? (char (symbol-name x) 0)))
  
(defun tprint (x)
  (cond ((symbolp x)
         (cond ((labelp x) (terpri) (prin1 x))
               ((not (refp x))
                (terpri)
                (princ "  ")
                (prin1 x)
                (princ #\space))
               (t (prin1 x)
                  (princ #\space))))
        ((listp x)
         (if (listp (car x))
             (pprint x)
           (dolist (i x) (tprint i))))
        (t (prin1 x) (princ #\space))))



(defun ctest0 ()
  (tprint (wam:defrel0 father ((father paul father-of-paul)))))

(defun ctest1 ()
  (tprint
   (wam:defrel0 father
           ((father paul father-of-paul))
           ((father son-of-paul paul))
           ((father daughter-of-paul paul)))))  

(defun ctest2 ()
  (tprint
   (wam:defrel0 grandfather
           ((grandfather ?x ?y) (father ?x ?z) (father ?z ?y)))))   

(defun ctest3 ()
  (tprint
   (wam:defrel0 p
           ((p ?X ?Y ?Z) (q ?U ?V ?W) (r ?Y ?Z ?U) (s ?U ?W) (t ?X ?V)))))
  
(defun ctest4 ()
  (tprint
   (wam:defrel0 p
           ((p #(f ?X) #(h ?Y #(f a)) ?Y) (father #(y ?X) #(z ?Y))))))


(defun ctest5 ()
  (tprint
   (defquery q ((father ?X ?Y)))))

(defun ctest6 ()
  (tprint
   (defquery q ((father ?X ?Y) (father ?Y ?X) (father ?Y ?Z)))))

(defun ctest7 ()
  (tprint
   (defquery q ((p ?Z (?Z ?W) #(f ?W))))))

(defun ctest9 ()
  (tprint
   (defquery q ((p (?X ?Y))))))

(defun ctest10 ()
  (tprint
   (defquery q ((p (?X))))))

(defun ctest11 ()
  (tprint
   (wam:defrel0 p ((p ?X ?Y) (p (?X ?Y))))))

(defun ctest12 ()
  (tprint
   (wam:defrel0 p ((p (?X ?Y))))))

(defun ltest1 ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel father
          ((father paul father-of-paul))
          ((father son-of-paul paul))
          ((father daughter-of-paul paul)))
  (if (equal '(((?X . "DAUGHTER-OF-PAUL") (?Y . "PAUL"))
               ((?X . "SON-OF-PAUL") (?Y . "PAUL"))
               ((?X . "PAUL") (?Y . "FATHER-OF-PAUL")))
             (wam:?- (father ?X ?Y)))
      'OK
    'FAILED))

(defun ltest2 ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel father
          ((father paul father-of-paul))
          ((father son-of-paul paul))
          ((father daughter-of-paul paul)))
  (wam:defrel grandfather
          ((grandfather ?child ?grandfather)
           (father ?child ?father) (father ?father ?grandfather)))
  (if (equal '(((?X . "DAUGHTER-OF-PAUL") (?Y . "FATHER-OF-PAUL")) ((?X . "SON-OF-PAUL") (?Y . "FATHER-OF-PAUL")))
             (wam:?- (grandfather ?X ?Y)))
      'OK
    'FAILED))

(defun ltest4 ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel father
          ((father paul father-of-paul))
          ((father son-of-paul paul))
          ((father daughter-of-paul paul)))
  (wam:defrel grandfather
          ((grandfather ?child ?grandfather)
           (father ?child ?father) (father ?father ?grandfather)))
  (if (equal '(((?X . "DAUGHTER-OF-PAUL") (?Y . "PAUL"))
               ((?X . "SON-OF-PAUL") (?Y . "PAUL"))
               ((?X . "PAUL") (?Y . "FATHER-OF-PAUL")))
             (wam:?- (father ?X ?Y)))
      'OK
    'FAILED))

(defun ltest5 ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel lll ((lll (1 2))))
  (if (equal (wam:?- (lll (?X ?Y)))
             '(((?X . 1) (?Y . 2))))
      'OK
    'FAILED))

(defun ltest6 ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel lis ((lis (1 2))))
  (if (equal (wam:?- (lis ?X)) '(((?X 1 2))))
      'OK
    'FAILED))

(defun ltest7 ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel struct ((struct #(a b c))))
  (let ((result (wam:?- (struct ?X))))
    (if (equalp
         result
         '(((?X . #("A/2" "B" "C")))))
        'OK
      (progn
        (format *error-output* "~&FAIL ~S~%" result)
        'FAIL))))

(defun ltest8 ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel struct ((struct #(a b c))))
  (if (equalp
       (wam:?- (struct #(a ?X ?Y)))
       '(((?X . "B") (?Y . "C"))))
      'OK
    'FAIL))

(defun ltest9 ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel struct
          ((struct #(a b c)))
          ((struct #(a e f)))
          ((struct #(g h i))))
  (if (equalp
       (wam:?- (struct #(a ?X ?Y)))
       '(((?X . "E") (?Y . "F")) ((?X . "B") (?Y . "C"))))
      'OK
    'FAIL))

;; this is illegal
;;;; (defun ltest10 ()
;;;;   (wam:init-opcodes)
;;;;   (wam:reset-code)
;;;;   (wam:reset-asm)
;;;;   (wam:defrel struct
;;;;           ((struct #(a b c)))
;;;;           ((struct #(a e f)))
;;;;           ((struct #(g h i))))
;;;;   (wam:?- (struct #(?X ?Y ?Z))))

(defun ltest10 ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel struct
          ((struct #(a b c)))
          ((struct #(a 1 2)))
          ((struct #(g h i))))
  (if (equalp
       (wam:?- (struct #(a ?X ?Y)))
       '(((?X . 1) (?Y . 2)) ((?X . "B") (?Y . "C"))))
      'OK
    'FAIL))

(defun ltest11 ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel bb
          ((bb 1 1))
          ((bb 2 3))
          ((bb 3 1))
          ((bb a b)))
  (if (equalp
       (wam:?- (bb ?id 1))
       '(((?ID . 3)) ((?ID . 1))))
      'OK
    'FAIL))

(defun ltest12 ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel bb
          ((bb 1 1))
          ((bb 2 3))
          ((bb 3 1))
          ((bb 4 2))
          ((bb 2 2))
          ((bb a b)))
  (wam:defrel get-bb
          ((get-bb ?X ?Y) (bb ?X ?Y) (bb ?Y ?X)))
  (if (equalp
       (wam:?- (get-bb ?id ?X))
       '(((?ID . 2) (?X . 2)) ((?ID . 1) (?X . 1))))
      'OK
    'FAIL))

(defun ltest13 ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel bb
          ((bb 1 1))
          ((bb 2 3))
          ((bb 3 1))
          ((bb 4 2))
          ((bb 2 2))
          ((bb a b)))
  (wam:defrel get-bb
          ((get-bb ?X ?Y) (bb ?X ?Y) ! (bb ?Y ?X)))
  (if (equalp
       (wam:?- (get-bb ?id ?X))
       '(((?ID . 1) (?X . 1))))
      'OK
    'FAIL))

(defun ltest13a ()
  ;; same as ltest13, but with fewer relations
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel bb
          ((bb 1 1))
          ((bb 2 2)))
  (wam:defrel get-bb
          ((get-bb ?X ?Y) (bb ?X ?Y) ! (bb ?Y ?X)))
 (if (equalp
      (wam:?- (get-bb ?id ?X))
      '(((?ID . 1) (?X . 1))))
     'OK
   'FAIL))

(defun ltest14 ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel bb
          ((bb 1 1))
          ((bb 2 3))
          ((bb 3 1))
          ((bb 4 2))
          ((bb 2 2))
          ((bb a b)))
  (wam:defrel get-bb
          ((get-bb ?X ?Y) ! (bb ?X ?Y) (bb ?Y ?X)))
  (let ((result (wam:?- (get-bb ?id ?X))))
    (if (equalp
         result
         '(((?ID . 1) (?X . 1))))
        'OK
      (progn
        (format *error-output* "~&FAIL ~S~%" result)
        'FAIL))))

(defun ltest15()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel bb
          ((bb 1 1))
          ((bb 2 3))
          ((bb 3 1))
          ((bb 4 2))
          ((bb 2 2))
          ((bb a b)))
  (wam:defrel get-bb
          ((get-bb ?X ?Y) (bb ?X 1) !)
          ((get-bb ?X ?Y) (bb ?X ?Y)))
  (wam:?- (get-bb ?id ?X)))


(defun ltest15a()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel bb
          ((bb 1 1))
          ((bb 2 3))
          ((bb 3 1))
          ((bb 4 2))
          ((bb 2 2))
          ((bb a b)))
  (wam:defrel get-bb
          ((get-bb ?X ?Y) (bb ?X 1) !))
  (wam:?- (get-bb ?id ?X)))
