(in-package :wam)

(defvar *code-io*) ;; class IO - used to hold compiled code

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

 ;; originally, wanted to make all of these variables "efficient", not specials

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

