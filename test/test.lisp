; Copyright 2005 Paul Tarvydas

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
  (tprint (defrel0 father ((father paul albin)))))

(defun ctest1 ()
  (tprint
   (defrel0 father
           ((father paul albin))
           ((father justin paul))
           ((father austina paul)))))  

(defun ctest2 ()
  (tprint
   (defrel0 grandfather
           ((grandfather ?x ?y) (father ?x ?z) (father ?z ?y)))))   

(defun ctest3 ()
  (tprint
   (defrel0 p
           ((p ?X ?Y ?Z) (q ?U ?V ?W) (r ?Y ?Z ?U) (s ?U ?W) (t ?X ?V)))))
  
(defun ctest4 ()
  (tprint
   (defrel0 p
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
   (defrel0 p ((p ?X ?Y) (p (?X ?Y))))))

(defun ctest12 ()
  (tprint
   (defrel0 p ((p (?X ?Y))))))

(defun ltest1 ()
  (init-opcodes)
  (reset-code)
  (reset-asm)
  (defrel father
          ((father paul albin))
          ((father justin paul))
          ((father austina paul)))
  (if (equal '(((?X . "AUSTINA") (?Y . "PAUL"))
               ((?X . "JUSTIN") (?Y . "PAUL"))
               ((?X . "PAUL") (?Y . "ALBIN")))
             (?- (father ?X ?Y)))
      'OK
    'FAILED))

(defun ltest2 ()
  (init-opcodes)
  (reset-code)
  (reset-asm)
  (defrel father
          ((father paul albin))
          ((father justin paul))
          ((father austina paul)))
  (defrel grandfather
          ((grandfather ?child ?grandfather)
           (father ?child ?father) (father ?father ?grandfather)))
  (if (equal '(((?X . "AUSTINA") (?Y . "ALBIN")) ((?X . "JUSTIN") (?Y . "ALBIN")))
             (?- (grandfather ?X ?Y)))
      'OK
    'FAILED))

(defun ltest4 ()
  (init-opcodes)
  (reset-code)
  (reset-asm)
  (defrel father
          ((father paul albin))
          ((father justin paul))
          ((father austina paul)))
  (defrel grandfather
          ((grandfather ?child ?grandfather)
           (father ?child ?father) (father ?father ?grandfather)))
  (if (equal '(((?X . "AUSTINA") (?Y . "PAUL"))
               ((?X . "JUSTIN") (?Y . "PAUL"))
               ((?X . "PAUL") (?Y . "ALBIN")))
             (?- (father ?X ?Y)))
      'OK
    'FAILED))

(defun ltest5 ()
  (init-opcodes)
  (reset-code)
  (reset-asm)
  (defrel lll ((lll (1 2))))
  (?- (lll (?X ?Y))))

(defun ltest6 ()
  (init-opcodes)
  (reset-code)
  (reset-asm)
  (defrel lis ((lis (1 2))))
  (?- (lis ?X)))

(defun ltest7 ()
  (init-opcodes)
  (reset-code)
  (reset-asm)
  (defrel struct ((struct #(a b c))))
  (?- (struct ?X)))

(defun ltest8 ()
  (init-opcodes)
  (reset-code)
  (reset-asm)
  (defrel struct ((struct #(a b c))))
  (?- (struct #(a ?X ?Y))))

(defun ltest9 ()
  (init-opcodes)
  (reset-code)
  (reset-asm)
  (defrel struct
          ((struct #(a b c)))
          ((struct #(a e f)))
          ((struct #(g h i))))
  (?- (struct #(a ?X ?Y))))

;; this is illegal
;;;; (defun ltest10 ()
;;;;   (init-opcodes)
;;;;   (reset-code)
;;;;   (reset-asm)
;;;;   (defrel struct
;;;;           ((struct #(a b c)))
;;;;           ((struct #(a e f)))
;;;;           ((struct #(g h i))))
;;;;   (?- (struct #(?X ?Y ?Z))))

(defun ltest10 ()
  (init-opcodes)
  (reset-code)
  (reset-asm)
  (defrel struct
          ((struct #(a b c)))
          ((struct #(a 1 2)))
          ((struct #(g h i))))
  (?- (struct #(a ?X ?Y))))

(defun ltest11 ()
  (init-opcodes)
  (reset-code)
  (reset-asm)
  (defrel bb
          ((bb 1 1))
          ((bb 2 3))
          ((bb 3 1))
          ((bb a b)))
  (?- (bb ?id 1)))

(defun ltest12 ()
  (init-opcodes)
  (reset-code)
  (reset-asm)
  (defrel bb
          ((bb 1 1))
          ((bb 2 3))
          ((bb 3 1))
          ((bb 4 2))
          ((bb 2 2))
          ((bb a b)))
  (defrel get-bb
          ((get-bb ?X ?Y) (bb ?X ?Y) (bb ?Y ?X)))
  (?- (get-bb ?id ?X)))

(defun ltest13 ()
  (init-opcodes)
  (reset-code)
  (reset-asm)
  (defrel bb
          ((bb 1 1))
          ((bb 2 3))
          ((bb 3 1))
          ((bb 4 2))
          ((bb 2 2))
          ((bb a b)))
  (defrel get-bb
          ((get-bb ?X ?Y) (bb ?X ?Y) ! (bb ?Y ?X)))
  (?- (get-bb ?id ?X)))

(defun ltest13a ()
  (init-opcodes)
  (reset-code)
  (reset-asm)
  (defrel bb
          ((bb 1 1))
          ((bb 2 2)))
  (defrel get-bb
          ((get-bb ?X ?Y) (bb ?X ?Y) ! (bb ?Y ?X)))
  (?- (get-bb ?id ?X)))

(defun ltest14 ()
  (init-opcodes)
  (reset-code)
  (reset-asm)
  (defrel bb
          ((bb 1 1))
          ((bb 2 3))
          ((bb 3 1))
          ((bb 4 2))
          ((bb 2 2))
          ((bb a b)))
  (defrel get-bb
          ((get-bb ?X ?Y) ! (bb ?X ?Y) (bb ?Y ?X)))
  (?- (get-bb ?id ?X)))

(defun ltest15()
  (init-opcodes)
  (reset-code)
  (reset-asm)
  (defrel bb
          ((bb 1 1))
          ((bb 2 3))
          ((bb 3 1))
          ((bb 4 2))
          ((bb 2 2))
          ((bb a b)))
  (defrel get-bb
          ((get-bb ?X ?Y) (bb ?X 1) !)
          ((get-bb ?X ?Y) (bb ?X ?Y)))
  (?- (get-bb ?id ?X)))


(defun ltest15a()
  (init-opcodes)
  (reset-code)
  (reset-asm)
  (defrel bb
          ((bb 1 1))
          ((bb 2 3))
          ((bb 3 1))
          ((bb 4 2))
          ((bb 2 2))
          ((bb a b)))
  (defrel get-bb
          ((get-bb ?X ?Y) (bb ?X 1) !))
  (?- (get-bb ?id ?X)))

;; sept. 15, 2006 - successfully runs tests to here
