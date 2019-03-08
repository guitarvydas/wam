; $Id: wam.lisp,v 1.4 2006/02/18 22:49:02 tarvydas Exp $
;; Copyright 2005 Paul Tarvydas

;; from http://wambook.sourceforge.net/ with permission (for free distribution)

;; L0 is a language that only does unification without backtracking
;; L0 has "queries" and "programs".  Compile a program, then query it.  Success if p unifies with q, else fail.
;; On success, variables in q will be bound to values from p.
;; L0 defines STRUCTURES and VARIABLES.  It puts these in the HEAP, using TAGS to identify the kind(s) of entities.
;; Unbound VARIABLES are represented as self-referential cells (i.e. point back to themselves).

;; L0 describes a memory layout (in the HEAP) and "register" usage (called Xn)
;; example: p(Z,h(Z,W),f(W)).
;; the HEAP will contain (ref. Fig2.1) : 
;;  0: STRUCT 1
;;  1: h/2
;;  2: REF 2
;;  3: REF 3
;;  4: STRUCT 5
;;  5: f/1
;;  6: REF 3
;;  7: STRUCT 8
;;  8: p/3
;;  9: REF 2
;; 10: STRUCT 1
;; 11: STRUCT 5
;;
;; this is inside-out (postfix) notation
;; h(Z,W) is represented in cells 0..3
;; 
;; Z is described in cell 2 and points to itself
;; another reference to Z is in cell 9, which points to cell 2 (all references to Z refer to cell 2, with the first
;; occurence being in cell 2, pointing to itself
;;
;; W is in cell 3 (and points to itself)
;; 
;; f(W) is in cells 5..6 and refers to cell 3 "W"
;;
;; p(... ... ...) is in cell 7 .. 11, pointing to cell 2 ("Z") and structs 1 and 5 


;; section 2.2 discusses compilation of *queries*, section 2.3 discusses compilation of *programs*+ 

(in-package :wam)

(proclaim '(optimize (debug 3) (safety 3) (speed 0) (space 0)))

(declaim (optimize (debug 3) (safety 3) (speed 0) (space 0)))

(defvar *code-io*) ;; class IO - used to hold compiled code

(defmacro untag (i)
  `(ash ,i -3))

(defmacro tag (i)
  `(logand ,i 7))

(defmacro tag-int (i)
  `(logior (ash ,i 3) int))

(defmacro tag-ref (i)
  `(logior (ash ,i 3) ref))

(defmacro tag-con (i)
  `(logior (ash ,i 3) con))

(defmacro tag-spcl (i)
  `(logior (ash ,i 3) spcl))

(defmacro tag-lis (i)
  `(logior (ash ,i 3) lis))

(defmacro tag-str (i)
  `(logior (ash ,i 3) str))


(defmacro next-byte ()
  `(prog1 (logand #xff (aref code p)) (incf p)))

; the store contains registers, followed by heap, followed by stack followed by trail stack
; all must be addressable by 20-bit indices - i.e. total size 1048576
;
; code and PDL stack are in different address spaces

(defconstant n-regs 256)
;(defconstant heap-size 786176)
;(defconstant stack-size 131072)
;(defconstant trail-size 131072)
(defconstant heap-size 8192)
(defconstant stack-size 4096)
(defconstant trail-size 3072)

(defconstant heap-start n-regs)
(defconstant stack-start (+ heap-start heap-size))
(defconstant trail-start (+ stack-start stack-size))

(defconstant store-size (+ n-regs heap-size stack-size trail-size))
(assert (>= #x100000 store-size))

(defconstant code-size 10240)
(defconstant pdl-size 1024)

;; most-positive-fixnum is 7fffff = 23 bits
;; store is made up of 23-bit entities
;; code is made up of bytes
;; immediate constants in code can be 1, 2, 3 bytes or indirect (2 bytes, hash index)

#|
(let ((p 0)
      (cp 0)
      (s 0)
      (h heap-start)
      (hb heap-start)
      (b stack-start)
      (b0 stack-start)
      (e stack-start)
      (tr trail-start)
      (store (make-array store-size :element-type 'fixnum))
      (code (make-array code-size :element-type '(integer 0 255) :initial-element 0))
      (pdl (make-array pdl-size :element-type 'fixnum :fill-pointer 0))
      (fail nil)
      (mode :read)
      (number-of-args 0))
  
  (declare (type fixnum p cp s h hb b b0 e tr number-of-args))
  |#

(defvar p 0)
(defvar cp 0)
(defvar s 0)
(defvar h heap-start)
(defvar hb heap-start)
(defvar b stack-start)
(defvar b0 stack-start)
(defvar e stack-start)
(defvar tr trail-start)
(defvar store (make-array store-size :element-type 'fixnum :initial-element 0))
(defvar code (make-array code-size :element-type '(integer 0 255) :initial-element 0))
(defvar pdl (make-array pdl-size :element-type 'fixnum :fill-pointer 0))
(defvar fail nil)
(defvar mode :read)
(defvar number-of-args 0)


  (defmacro store (n)
    `(aref store ,n))

  (defmacro heap (n) `(store ,n))
  (defmacro regx (n) `(store ,n))
  (defmacro rega (n) `(store ,n))
  (defmacro var (n) `(store ,n))
  (defmacro stack (n) `(store ,n))
  (defmacro trail-stack (n) `(store ,n))
  (defmacro local (n) `(store (+ e ,n 1)))

  (defun next-triple ()
    (let* ((c1 (next-byte))
           (c2 (next-byte))
           (c3 (next-byte)))
      (logior (ash c1 16) (ash c2 8) c3)))

  (defun next-double ()
    (let* ((c1 (next-byte))
           (c2 (next-byte)))
      (logior (ash c1 8) c2)))

  (defun next-label ()
    (next-triple))

(defun next-byte-const ()
  (next-byte))

(defun next-word-const ()
  (next-double))

(defun next-tri-const ()
  (next-triple))

(defun next-const ()
  (next-double))

  (defun next-abs ()
    ;; next absolute address in code space (no arity)
    (next-double))


  ; a label is 20 bits - 15 msb for address, 5 lsb for arity
  (defmacro arity (x)
    `(logand ,x 31))

  (defmacro code-addr (x)
    `(ash ,x -5))

  (defmacro pdl-push (x)
    `(vector-push pdl ,x))

  (defmacro pdl-pop ()
    `(vector-pop pdl))

  (defmacro pdl-empty ()
    `(= 0 (fill-pointer pdl)))

  (defun reset-wam ()
    (setf p 0
          cp -1
          s 0
          h heap-start
          hb heap-start
          b (1- stack-start)
          b0 (1- stack-start)
          e stack-start
          tr trail-start
          fail nil
          mode :read
          number-of-args 0
          (fill-pointer pdl) 0)
    (loop for i from 0 below store-size do
          (setf (aref store i) 0)))

(defun interp-wam (start symbols &optional display)
  (let (result)
    (reset-wam)
    (setf p (code-addr start))
    (when display
      (format t "~A~%" (logand #xff (aref code p))))
    (catch 'quit
      (loop while (>= p 0)
        do (let ((byte (next-byte)))
             (declare (type (integer 0 255) byte))
             (when display
               (format t "~A~%" (disassem byte)))
             (if (= #.done byte)
                 (progn
                   (when symbols
                     (push (mapcar #'(lambda (pair)
                                       (cons (car pair) (fetch pair)))
                                   symbols)
                           result))
                   (backtrack))
               (funcall (aref opcode-array byte) byte))
             (when display
               (dump)
               (format t "~%")))))
    result))


(defun f-put-x-variable (byte) (declare (ignorable byte))
  (let* ((x (next-byte))
         (a (next-byte))
         (v (tag-ref h)))
    (setf (heap h) v
          (regx x) v
          (rega a) v)
    (incf h)))

(defun f-put-y-variable (byte) (declare (ignorable byte))
  (let* ((addr (+ e (next-byte) 1))
         (i (next-byte))
         (v (tag-ref addr)))
    (setf (stack addr) v
          (rega i) v)))

(defun f-put-x-value (byte) (declare (ignorable byte))
  (let* ((n (next-byte))
         (i (next-byte)))
    (setf (rega i) (store n))))

(defun f-put-y-value (byte) (declare (ignorable byte))
  (let* ((n (next-byte))
	 (i (next-byte))
	 (addr (deref (+ e n 1))))
    (setf (rega i) (store addr))))

(defun f-put-y-unsafe-value (byte) (declare (ignorable byte))
  (let* ((n (next-byte))
	 (i (next-byte))
	 (addr (deref (+ e n 1))))
    (if (< addr e)
	(setf (rega i) (store addr))
      (let ((v (tag-ref h)))
	(setf (heap h) v)
	(bind addr h)
	(setf (rega i) v)
	(incf h)))))

(defun f-put-structure (byte) (declare (ignorable byte))
  ;; a structure on the heap is a constant (name+arity of struct)
  ;; followed by fields of struct (n = arity)
  (let* ((fn (next-const))
	 (i (next-byte)))
    (setf (heap h) fn
	  (rega i) (tag-str h))
    (incf h)))

(defun f-put-list (byte) (declare (ignorable byte))
  (setf (rega (next-byte)) (tag-lis h)))

(defun f-put-nil (byte) (declare (ignorable byte))
  (setf (rega (next-byte)) (tag-spcl 0)))

(defun f-put-byte-constant (byte) (declare (ignorable byte))
  (let* ((c (next-byte-const))
	 (i (next-byte)))
    (setf (rega i) (tag-int c))))

(defun f-put-word-constant (byte) (declare (ignorable byte))
  (let* ((c (next-word-const))
	 (i (next-byte)))
    (setf (rega i) (tag-int c))))

(defun f-put-tri-constant (byte) (declare (ignorable byte))
  (let* ((c (next-tri-const))
	 (i (next-byte)))
    (setf (rega i) (tag-int c))))

(defun f-put-constant (byte) (declare (ignorable byte))
  (let* ((c (next-const))
	 (i (next-byte)))
    (setf (rega i) (tag-con c))))

(defun f-get-x-variable (byte) (declare (ignorable byte))
  (let* ((n (next-byte))
	 (i (next-byte)))
    (setf (store n) (rega i))))

(defun f-get-y-variable (byte) (declare (ignorable byte))
  (let* ((addr (+ e (next-byte) 1))
	 (i (next-byte)))
    (setf (store addr) (rega i))))

(defun f-get-x-value (byte) (declare (ignorable byte))
  (let* ((n (next-byte))
	 (i (next-byte)))
    (unify (store n) (rega i))
    (when fail
      (backtrack))))
                  
(defun f-get-y-value (byte) (declare (ignorable byte))
  (let* ((addr (+ e (next-byte) 1))
	 (i (next-byte)))
    (unify (store addr) (rega i))
    (when fail
      (backtrack))))
                  
(defun f-get-structure (byte) (declare (ignorable byte))
  (let* ((fn (next-const))
	 (i (next-byte))
	 (addr (deref i))
	 (tag (tag (store addr))))
    (setf fail nil)
    (case tag
      (#.ref
       (setf (heap h) (tag-str (1+ h)))
       (setf (heap (1+ h)) (tag-con fn))
       (bind addr h)
       (incf h 2)
       (setf mode :write))
      (#.str
       (let ((a (untag (store addr))))
         (if (= (heap a) fn)
             (setf s (1+ a)
                   mode :read)
           (setf fail t))))
      (otherwise
       (setf fail t)))
    (when fail
      (backtrack))))

(defun f-get-list (byte) (declare (ignorable byte))
  (let* ((i (next-byte))
	 (addr (deref i))
	 (tag (tag (store addr))))
    (setf fail nil)
    (case tag
      (#.ref
       (setf (heap h) (tag-lis (1+ h)))
       (bind addr h)
       (incf h)
       (setf mode :write))
      (#.lis
       (let ((a (untag (store addr))))
         (setf s a
               mode :read)))
      (otherwise
       (setf fail t)))
    (when fail
      (backtrack))))


(defun f-get-byte-constant (byte) (declare (ignorable byte))
  (let* ((c (next-byte-const))
	 (i (next-byte))
	 (addr (deref i))
	 (tag (tag (store addr))))
    (setf fail nil)
    (case tag
      (#.ref
       (setf (store addr) (tag-int c))
       (trail addr))
      (#.int
       (let ((c1 (untag (store addr))))
         (setf fail (or (/= int (tag (store addr)))
                        (/= c c1)))))
      (otherwise
       (setf fail t)))
    (when fail
      (backtrack))))

(defun f-get-word-constant (byte) (declare (ignorable byte))
  (let* ((c (next-word-const))
	 (i (next-byte))
	 (addr (deref i))
	 (tag (tag (store addr))))
    (setf fail nil)
    (case tag
      (#.ref
       (setf (store addr) (tag-int c))
       (trail addr))
      (#.int
       (let ((c1 (untag (store addr))))
         (setf fail (or (/= int (tag (store addr)))
                        (/= c c1)))))
      (otherwise
       (setf fail t)))
    (when fail
      (backtrack))))

(defun f-get-tri-constant (byte) (declare (ignorable byte))
  (let* ((c (next-tri-const))
	 (i (next-byte))
	 (addr (deref i))
	 (tag (tag (store addr))))
    (setf fail nil)
    (case tag
      (#.ref
       (setf (store addr) (tag-int c))
       (trail addr))
      (#.int
       (let ((c1 (untag (store addr))))
         (setf fail (or (/= int (tag (store addr)))
                        (/= c c1)))))
      (otherwise
       (setf fail t)))
    (when fail
      (backtrack))))

(defun f-get-constant (byte) (declare (ignorable byte))
  (let* ((c (next-const))
	 (i (next-byte))
	 (addr (deref i))
	 (tag (tag (store addr))))
    (setf fail nil)
    (case tag
      (#.ref
       (setf (store addr) (tag-con c))
       (trail addr))
      (#.con
       (let ((c1 (untag (store addr))))
         (setf fail (or (/= con (tag (store addr)))
                        (/= c c1)))))
      (otherwise
       (setf fail t)))
    (when fail
      (backtrack))))

(defun f-get-nil (byte) (declare (ignorable byte))
  (let* ((i (next-byte))
	 (addr (deref i))
	 (tag (tag (store addr))))
    (setf fail nil)
    (case tag
      (#.ref
       (setf (store addr) (tag-spcl 0))
       (trail addr))
      (#.spcl
       (let ((c1 (untag (store addr))))
         (setf fail (/= 0 c1))))
      (otherwise
       (setf fail t)))
    (when fail
      (backtrack))))

(defun f-set-x-variable (byte) (declare (ignorable byte))
  (let* ((n (next-byte))
	 (v (tag-ref h)))
    (setf (heap h) v
	  (var n) v)
    (incf h)))

(defun f-set-x-value (byte) (declare (ignorable byte))
  (let* ((n (next-byte)))
    (setf (heap h) (regx n))
    (incf h)))

(defun f-set-y-variable (byte) (declare (ignorable byte))
  (let* ((addr (+ e (next-byte) 1))
	 (v (tag-ref addr)))
    (setf (heap h) v)
    (setf (stack addr) v)
    (incf h)))

(defun f-set-y-value (byte) (declare (ignorable byte))
  (let* ((a (next-triple))
	 (addr (deref a)))
    (if (< addr h)
	(setf (heap h) (heap addr))
      (progn
	(setf (heap h) (tag-ref h))
	(bind addr h)))
    (incf h)))

(defun f-set-byte-constant (byte) (declare (ignorable byte))
  (setf (heap h) (tag-int (next-byte-const)))
  (incf h))

(defun f-set-word-constant (byte) (declare (ignorable byte))
  (setf (heap h) (tag-int (next-word-const)))
  (incf h))

(defun f-set-tri-constant (byte) (declare (ignorable byte))
  (setf (heap h) (tag-int (next-tri-const)))
  (incf h))

(defun f-set-constant (byte) (declare (ignorable byte))
  (setf (heap h) (tag-con (next-const)))
  (incf h))

(defun f-set-void (byte) (declare (ignorable byte))
  (let ((n (next-byte)))
    (loop for i from h below (+ h n)
      do (setf (heap i) (tag-ref i)))
    (incf h n)))

(defun f-unify-x-variable (byte) (declare (ignorable byte))
  (let ((n (next-byte)))
    (ecase mode
      (:read (setf (regx n) (heap s)))
      (:write
       (setf (regx n)
             (setf (heap h) (tag-ref h)))
       (incf h)))
    (incf s)))

(defun f-unify-x-value (byte) (declare (ignorable byte))
  (setf fail nil)
  (let ((n (next-byte)))
    (ecase mode
      (:read (unify (regx n) s))
      (:write
       (let ((addr (deref n)))
         (if (< addr h)
             (setf (heap h) (heap addr))
           (progn
             (setf (heap h) (tag-ref h))
             (bind addr h))))
       (incf h)))
    (incf s))
  (when fail
    (backtrack)))

(defun unify-word-constant (byte)
  (f-unify-byte-constant byte))

(defun unify-tri-constant (byte)
  (f-unify-byte-constant byte))

(defun f-unify-byte-constant (byte) (declare (ignorable byte))
  (let ((c (ecase byte
             (#.unify-byte-constant (next-byte-const))
             (#.unify-word-constant (next-word-const))
             (#.unify-tri-constant (next-tri-const)))))
    (setf fail nil)
    (ecase mode
      (:read
       (let* ((addr (deref s))
              (tag (tag (store addr))))
         (incf s)
         (case tag
           (#.ref
            (setf (store addr) (tag-int c))
            (trail addr))
           (#.int
            (setf fail (or (/= int (tag (store addr)))
                           (/= c (untag (store addr))))))
           (otherwise
            (setf fail t)))))
      (:write
       (setf (heap h) (tag-int c))
       (incf h)))
    (when fail
      (backtrack))))

(defun f-unify-constant (byte) (declare (ignorable byte))
  (let ((c (next-const)))
    (setf fail nil)
    (ecase mode
      (:read
       (let* ((addr (deref s))
              (tag (tag (store addr))))
         (incf s)
         (case tag
           (#.ref
            (setf (store addr) (tag-con c))
            (trail addr))
           (#.con
            (setf fail (or (/= con (tag (store addr)))
                           (/= c (untag (store addr))))))
           (otherwise
            (setf fail t)))))
      (:write
       (setf (heap h) (tag-con c))
       (incf h)))
    (when fail
      (backtrack))))

(defun f-unify-void (byte) (declare (ignorable byte))
  (let ((n (next-byte)))
    (ecase mode
      (:read (incf s n))
      (:write
       (loop for i from h below (+ h n)
         do (setf (heap i) (tag-ref i)))
       (incf h n)))))

(defun f-allocate (byte) (declare (ignorable byte))
  (let ((newe
	 (cond ((= -1 cp)
		e)
	       ((> e b)
		(+ e (aref code (1- cp)) 2))
	       (t (+ b (stack b) 8)))))
    (setf (stack newe) e
	  (stack (1+ newe)) cp)
    (setf e newe)))

(defun f-deallocate (byte) (declare (ignorable byte))
  (setf cp (stack (1+ e)))
  (setf e (stack e)))

(defun f-call (byte) (declare (ignorable byte))
  (let* ((proc (next-label))
	 (n (next-byte)))
    (declare (ignorable n))
    (if (defined proc)
	(let ()
	  (setf number-of-args (arity proc))
	  (setf cp p)
	  (setf b0 b)
	  (setf p (code-addr proc)))
      (backtrack))))

(defun f-execute (byte) (declare (ignorable byte))
  (let ((proc (next-label)))
    (if (defined proc)
	(let ()
	  (setf number-of-args (arity proc))
	  (setf b0 b)
	  (setf p (code-addr proc)))
      (backtrack))))

(defun f-proceed (byte) (declare (ignorable byte))
  (setf p cp))

(defun f-try-me-else (byte) (declare (ignorable byte))
  (let* ((L (next-abs))
	 (newB
	  (if (> e b)
	      (+ e (aref code (1- cp)) 2)
	    (+ b (stack b) 8)))
	 (n number-of-args))
    (setf (stack newB) n)
    (loop for i from 1 to n
      do (setf (stack (+ newB i)) (rega i)))
    (let ((bn (+ newB n)))
      (setf (stack (+ bn 1)) e
	    (stack (+ bn 2)) cp
	    (stack (+ bn 3)) b
	    (stack (+ bn 4)) L
	    (stack (+ bn 5)) tr
	    (stack (+ bn 6)) h
	    (stack (+ bn 7)) b0
	    b newB
	    hb h))))

(defun f-retry-me-else (byte) (declare (ignorable byte))
  (let* ((L (next-abs))
	 (n (stack b)))
    (loop for i from 1 to n
      do (setf (rega i) (stack (+ b i))))
    (setf e (stack (+ b n 1))
	  cp (stack (+ b n 2))
	  (stack (+ b n 4)) L)
    (unwind-trail (stack (+ b n 5)) tr)
    (setf tr (stack (+ b n 5))
	  h (stack (+ b n 6))
	  hb h)))

(defun f-trust-me (byte) (declare (ignorable byte))
  (let ((n (stack b)))
    (loop for i from 1 to n
      do (setf (rega i) (stack (+ b i))))
    (setf e (stack (+ b n 1))
	  cp (stack (+ b n 2)))
    (unwind-trail (stack (+ b n 5)) tr)
    (setf tr (stack (+ b n 5))
	  h (stack (+ b n 6))
	  b (stack (+ b n 3))
	  hb (stack (+ b n 6)))))

(defun f-try (byte) (declare (ignorable byte))
  (let ((L (next-label))
	(newB (if (> e b)
		  (+ e (aref code (1- cp)) 2)
		(+ b (stack b) 8)))
	(n number-of-args))
    (setf (stack newB) n)
    (loop for i from 1 to n
      do (setf (stack (+ newB i)) (rega i)))
    (let ((bn (+ newB n)))
      (setf (stack (+ bn 1)) e
	    (stack (+ bn 2)) cp
	    (stack (+ bn 3)) b
	    (stack (+ bn 4)) p
	    (stack (+ bn 5)) tr
	    (stack (+ bn 6)) h
	    (stack (+ bn 7)) b0
	    b newB
	    hb h
	    p (code-addr L)))))

(defun f-retry (byte) (declare (ignorable byte))
  (let ((L (next-label))
	(n (stack b)))
    (loop for i from 0 to (1- n)
      do (setf (rega i) (stack (+ b i))))
    (setf e (stack (+ b n 1))
	  cp (stack (+ b n 2))
	  (stack (+ b n 4)) p)
    (unwind-trail (stack (+ b n 5)) tr)
    (setf tr (stack (+ b n 5))
	  h (stack (+ b n 6))
	  hb h
	  p (code-addr L))))

(defun f-trust (byte) (declare (ignorable byte))
  (let ((L (next-label))
	(n (stack b)))
    (loop for i from 1 to n
      do (setf (rega i) (stack (+ b i))))
    (setf e (stack (+ b n 1))
	  cp (stack (+ b n 2)))
    (unwind-trail (stack (+ b n 5)) tr)
    (setf tr (stack (+ b n 5))
	  h (stack (+ b n 6))
	  b (stack (+ b n 3))
	  hb (stack (+ b n 6))
	  p (code-addr L))))

(defun f-switch-on-term (byte) (declare (ignorable byte))
  (let* ((lv (next-label))
	 (lc (next-label))
	 (ll (next-label))
	 (ls (next-label)))
    (setf p (code-addr
	     (ecase (tag (store (deref 1)))
               (#.ref lv)
               (#.con lc)
               (#.lis ll)
               (#.str ls))))))

;;; (defun f-switch-on-constant (byte) (declare (ignorable byte))
;;;   (let* ((c (store (deref 1)))
;;; 	 (val (untag c))
;;; 	 (n (next-byte))
;;; 	 (table (next-double)))
;;;     (declare (ignorable n))
;;;     (multiple-value-bind (found code-addr)
;;;         (gethash val (table table))
;;;       (if found
;;;           (setf p code-addr)
;;;         (backtrack)))))


;;; (defun f-switch-on-structure (byte) (declare (ignorable byte))
;;;   (let* ((c (store (deref 1)))
;;; 	 (val (untag c))
;;; 	 (n (next-byte))
;;; 	 (table (next-double)))
;;;     (declare (ignorable n))
;;;     (multiple-value-bind (found code-addr)
;;;         (gethash val (table table))
;;;       (if found
;;;           (setf p code-addr)
;;;         (backtrack)))))

(defun f-neck-cut (byte) (declare (ignorable byte))
  (when (> b b0)
    (setf b b0)
    (tidy-trail)))

(defun f-get-level (byte) (declare (ignorable byte))
  (let ((n (next-byte)))
    (setf (stack (+ e 1 n)) b0))
  nil)

(defun f-cut (byte) (declare (ignorable byte))
  (let ((n (next-byte)))
    (when (> b (stack (+ e 1 n)))
      (setf b (stack (+ e 1 n)))
      (tidy-trail))))



(defun fetch (pair)
  ; given a symbol-allocation pair, fetch the
  ; value of the symbol and return it
  (let ((r (alloc-reg (cdr pair))))
    (if (alloc-is-temp (cdr pair))
        (fetch-temp r)
      (fetch-local r))))

(defun fetch-temp (r)
  (fetch-store (store (deref r))))

(defun fetch-local (r)
  (if (<= 0 (local r) (1- store-size))
      (fetch-store (deref (local r)))
    (format nil "unbound ~A" (untag (local r)))))

(defun fetch-store (v)
  ;; helper that extracts values from the environment and converts
  ;; them to lisp so that they can be returned by DONE
  (ecase (tag v)
    (#.int (untag v))
    (#.ref (format nil "unbound ~A" (untag v)))
    (#.con (gethash (untag v) (unconsts)))
    (#.lis (list (fetch-store (store (untag v)))  ; a lis is always car/cdr (a pair)
                 (fetch-store (store (1+ (untag v))))))
    (#.str
     ; first const is the struct and its name contains the arity
     (let* ((a (untag v))
            (struct-con (store a)))
       (assert (= con (tag struct-con)))
       (let* ((const (untag struct-con))
              (name-arity (gethash const (unconsts)))
              (arity (extract-arity-from-const name-arity))
              (result (make-array (1+ arity)))
              (i 1))
         (setf (aref result 0) name-arity)
         (incf a)
         (dotimes (j arity)
           (setf (aref result i) (fetch-store (store a)))
           (incf a)
           (incf i))
         result)))
    (#.spcl (assert (zerop (untag v))) "nil")))

(defun extract-arity-from-const (name-arity-string)
  (parse-integer (subseq name-arity-string
                         (1+ (position #\/ name-arity-string)))))

(defun backtrack ()
  (if (< b stack-start)
      (throw 'quit nil)
    (progn
      (setf b0 (stack (+ b (stack b) 7))
            p (stack (+ b (stack b) 4))))))

(defun deref (a)
  (let* ((v (store a))
         (tag (tag v))
         (val (untag v)))
    (declare (type fixnum a v tag val))
    (if (and (= tag ref) (/= val a))
        (deref val)
      (the fixnum a))))

  (defun bind (a1 a2)
    (declare (type fixnum v1 v2 t1 a1 a2))
    (let* ((v1 (store a1))
           (v2 (store a2))
           (t1 (tag v1))
           (t2 (tag v2)))
      (if (and (= t1 ref)
               (or (/= t2 ref) (< a2 a1)))
          (progn
            (setf (store a1) (store a2))
            (trail a1))
        (progn
          (setf (store a2) (store a1))
          (trail a2)))))

(defun trail (a)
  (declare (type fixnum a))
  (when (or (< a hb)
            (and (< h a) (< a b)))
    (setf (trail-stack tr) a)
    (incf tr)))

(defun unwind-trail (a1 a2)
  (declare (type fixnum a1 a2))
  (loop for i from a1 to (1- a2)
        do (setf (store (trail-stack i)) (tag-ref (trail-stack i)))))

(defun tidy-trail ()
  (unless (< b stack-start)
    (loop with i = (stack (+ b (stack b) 5))
          while (< i tr)
          do (if (or (< (trail-stack i) hb)
                     (and (< h (trail-stack i)) (< (trail-stack i) b)))
                 (incf i)
               (progn
                 (setf (trail-stack i) (trail-stack (1- tr)))
                 (decf tr))))))

  (defun unify (a1 a2)
    (declare (type fixnum a1 a2))
    (pdl-push a1)
    (pdl-push a2)
    (setf fail nil)
    (loop while (not (or (pdl-empty) fail)) do
      (let ((d1 (deref (pdl-pop)))
            (d2 (deref (pdl-pop))))
        (declare (type fixnum d1 d2))
        (unless (= d1 d2)
          (let* ((s1 (store d1))
                 (t1 (tag s1))
                 (v1 (untag s1))
                 (s2 (store d2))
                 (t2 (tag s2))
                 (v2 (untag s2)))
            (declare (type fixnum s1 t1 v1 s2 t2 v2))
            (if (= ref t1)
                (bind d1 d2)
              (ecase t2
                (#.ref (bind d1 d2))
                (#.con (setf fail (or (/= t1 con) (/= v1 v2))))
                (#.int (setf fail (or (/= t1 con) (/= v1 v2))))
                (#.lis (if (/= t1 lis)
                           (setf fail t)
                         (progn
                           (pdl-push v1)
                           (pdl-push v2)
                           (pdl-push (1+ v1))
                           (pdl-push (1+ v2)))))
                (#.str (if (/= str t1)
                           (setf fail t)
                         (let ((fn1 (store v1))
                               (fn2 (store v2)))
                           (declare (type fixnum fn1 fn2))
                           (if (/= fn1 fn2)
                               (setf fail t)
                             (loop for i from 1 to (arity fn1) do
                               (pdl-push (+ v1 i))
                               (pdl-push (+ v2 i))))))))))))))

;  (let (code-p procs consts id)
(defvar code-p)
(defvar procs)
(defvar consts)
(defvar unconsts)
(defvar id)

(defun unconsts () unconsts)

(defun reset-code ()
  (if (boundp 'procs)
      (clrhash procs)
    (setf procs (make-hash-table :test 'eq)))
  (if (boundp 'consts)
      (clrhash consts)
    (setf consts (make-hash-table :test 'equal)))
  (if (boundp 'unconsts)
      (clrhash consts)
    (setf unconsts (make-hash-table :test 'eq)))
  (setf id 0)
  (setf (gethash "NIL" consts) 0)
  (setf (gethash 0 unconsts) "NIL")
  (setf code-p 0)
  (setf *code-io* (make-instance 'array-io :array code)))

(defun next-id ()
  (incf id))

(defun w-tell () code-p)

(defun w-byte (x)
  (setf (aref code code-p) x)
  (incf code-p))

(defun w-opcode (x)
  (w-byte x))

(defun w-reg (n)
  (w-byte n))

(defun w-triple (x)
  (w-byte (logand (ash x -16) #xff))
  (w-byte (logand (ash x -8) #xff))
  (w-byte (logand x #xff)))

(defun w-const (c)
  (w-triple c))

(defun w-label (f &optional n)
  (if n
      (w-triple (logior (ash f 5) (logand n 31)))
    (w-triple f)))

(defun make-proc (pc name arity)
  (let ((lab (logior (ash pc 5) (logand arity 31))))
    (assert (null (gethash lab procs)))
    (setf (gethash lab procs) name)
    lab))

(defun fetch-constant (c)
  (or (gethash c consts)
      (let ((id (next-id)))
        (setf (gethash c consts) id)
        (setf (gethash id unconsts) c)
        id)))

(defun defined (label)
  (multiple-value-bind (val success)
      (gethash label procs)
    (declare (ignore val))
    success))

(defun code ()
  code)

;)
