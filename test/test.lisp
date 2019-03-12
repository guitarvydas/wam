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
  (wam:?- (lll (?X ?Y))))

(defun ltest6 ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel lis ((lis (1 2))))
  (wam:?- (lis ?X)))

(defun ltest7 ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel struct ((struct #(a b c))))
  (wam:?- (struct ?X)))

(defun ltest8 ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel struct ((struct #(a b c))))
  (wam:?- (struct #(a ?X ?Y))))

(defun ltest9 ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel struct
          ((struct #(a b c)))
          ((struct #(a e f)))
          ((struct #(g h i))))
  (wam:?- (struct #(a ?X ?Y))))

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
  (wam:?- (struct #(a ?X ?Y))))

(defun ltest11 ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel bb
          ((bb 1 1))
          ((bb 2 3))
          ((bb 3 1))
          ((bb a b)))
  (wam:?- (bb ?id 1)))

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
  (wam:?- (get-bb ?id ?X)))

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
  (wam:?- (get-bb ?id ?X)))

(defun ltest13a ()
  (wam:init-opcodes)
  (wam:reset-code)
  (wam:reset-asm)
  (wam:defrel bb
          ((bb 1 1))
          ((bb 2 2)))
  (wam:defrel get-bb
          ((get-bb ?X ?Y) (bb ?X ?Y) ! (bb ?Y ?X)))
  (wam:?- (get-bb ?id ?X)))

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
  (wam:?- (get-bb ?id ?X)))

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

;; sept. 15, 2006 - successfully runs tests to here
