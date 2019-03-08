; $Id: asm.lisp,v 1.2 2006/02/08 05:28:12 tarvydas Exp $
; Copyright 2005 Paul Tarvydas

;;
;; assembler
;;
;; integer -> write one byte
;; string -> write a constant id (2 bytes), install constant into table
;; %integer -> write two bytes
;; &integer -> write three bytes
;; ^integer -> write four bytes
;; 'c -> write one character
;; $name -> label definition
;; ?name -> label reference absolute
;; if the name has a / in it, it is a prolog proc+arity name:
;; $name/n -> proc+arity definition, installs proc using "make-proc"
;; ?name/n -> proc+arity reference absolute
;; (base name) -> label reference, base-relative
;; (base number) -> label + number
;; (= name number) -> equate name to number (byte)
;;
;; uses objects of type io for input and output

(in-package :wam)

(defvar *pc* 0)
(defvar *labels* nil)
(defvar *pc-start* 0)

(defun symbol-first-char (sym)
  (and (symbolp sym) (char (symbol-name sym) 0)))

(define-condition asm-error (error)
  ((message :initarg :message :reader message))
  (:report (lambda (condition stream)
             (format stream "Assembler error: ~A" (message condition)))))

(defun error-if (pred msg &rest ignore)
  (declare (ignore ignore))
  (when pred
    (error msg)))

(defun reset-asm ()
  (if *labels*
      (clrhash *labels*)
    (setf *labels* (make-hash-table :test 'equal)))
  (setf *pc* 0
        *pc-start* 0))


(defun assemble (in out)
  ;; pass 1 - collect labels, go through motions of emitting
  (progn
    (setf *pc-start* *pc*)
    (setf *pc* *pc-start*)
    (loop for item = (next-element in)
          while item do
          (assemble-1 item nil nil))
    ;; pass 2
    (reset in)
    (setf *pc* *pc-start*)
    (loop for item = (next-element in)
          while item do
          (assemble-1 item out t))))

(defun assemble-1 (item outf emit) ;; emit = nil on pass 1, = t on pass 2
  (cond

   ((numberp item) (out-byte item outf emit)
                   (incf *pc*))

   ((characterp item) (out-byte (char-code item) outf emit)
                     (incf *pc*))

   ((listp item)
    (if (eq '= (first item))
        ;; equate
        (progn
          (unless emit (assert (null (gethash (second item) *labels*))))
          (setf (gethash (second item) *labels*) (third item)))
      (if (eq 'quote (first item))
          ;; char emit
          (progn (out-byte (char-code (char (format nil "~A" (second item)) 0))
                           outf emit) (incf *pc*))

        (let ((base (first item)) (name (second item)))
          (multiple-value-bind (b success) (gethash base *labels*)
            (error-if (and emit (not success)) (format nil "ASM: base label ~A is not defined" base)
                      :ex 'asm-error)
            (if (numberp name)
                (if emit (let ((diff (+ b name)))
                           (setf *pc* (+ *pc* (calc-word-size diff)))
                           (out-flexi-num diff outf emit))
                  (setf *pc* (+ *pc* (calc-word-size 0))))
              
              (multiple-value-bind (v success) (gethash name *labels*)
                (error-if (and emit (not success)) (format nil "ASM: label ~A is not defined in file" name) :ex 'asm-error)
;(when (and emit (null v)) (break))
                (if emit (let ((diff (- (if v v 0) b)))
                           (setf *pc* (+ *pc* (calc-word-size diff)))
                           (out-flexi-num diff outf emit))
                  (setf *pc* (+ *pc* (calc-word-size 0)))))))))))
    
   ((symbolp item)
    (case (symbol-first-char item)

      (#\% (setf *pc* (+ (out-word (strip-to-int item emit) outf emit) *pc*)))

      (#\& (setf *pc* (+ (out-three (strip-to-int item emit) outf emit) *pc*)))
      (#\^ (setf *pc* (+ (out-long (strip-to-int item emit) outf emit) *pc*)))

      ; label definition - active only on first pass
      (#\$ (when (not emit)
             (let ((name (strip-to-symbol item)))
               (multiple-value-bind (v success) (gethash name *labels*) (declare (ignore v))
                 (error-if success (format nil "ASM: label ~A already defined" name)))
               (setf (gethash name *labels*)
                     (if (prolog-p name)
                         (multiple-value-bind (name-part arity)
                             (prolog-split name)
                           (make-proc *pc* name-part arity))
                       *pc*)))))

      ; label reference - all labels are pc-relative
      (#\? (let ((name (strip-to-symbol item)))
             (multiple-value-bind (v success) (gethash name *labels*)
             (error-if (and emit (not success)) (format nil "ASM: label ~A is not defined" name) :ex 'asm-error)
             (if (prolog-p name)
                 (progn
                   (incf *pc* 3)
                   (out-three v outf emit))
               (progn
                 (incf *pc* (calc-word-size v))
                 (out-word v outf emit))))))

      (otherwise (setf *pc* (+ (assemble-opcode item outf emit) *pc*)))))

   ((stringp item)
    ;; use external call - get-constant - to install constant into table return an id
    (let ((id (fetch-constant item)))
      (incf *pc* (out-word id outf emit))))

   (t (assert nil))))

(defun asm-get-name (name)
  (gethash name *labels*))

(defun prolog-p (s)
  (position #\/ (symbol-name s)))

(defun prolog-split (s)
  (let* ((n (symbol-name s))
         (p (position #\/ n)))
    (values (subseq n 0 p)
            (parse-integer (subseq n (1+ p))))))

(defun assemble-opcode (item outf emit)
  (multiple-value-bind (v succ) (gethash item *opcodes*)
    (error-if (not succ) (format nil "ASM: opcode ~A is not defined" item) :ex 'asm-error)
    (out-byte v outf emit))
  1)

(defun strip-to-int (item emit)
  "remove 1st character from symbol, then return it as an integer
   if second character is ? then resolve the label to an integer"
  (with-input-from-string (s (subseq (symbol-name item) 1))
    (let ((r (read s)))
      (when (symbolp r)
        (let ((name (strip-to-symbol r)) (temp-pc 0))
          (multiple-value-bind (v success) (gethash name *labels*)
            (error-if (and emit (not success)) (format nil "ASM: label ~A is not defined" name) :ex 'asm-error)
            (setf temp-pc (+ *pc* (calc-word-size v)))
            (setf r (if v (- v temp-pc) 0)))))
      r)))

(defun strip-to-symbol (item)
  "remove 1st character from symbol, then return it as a string"
  (intern (subseq (symbol-name item) 1)))

(defun out-word (item outf emit)
  (when emit 
    (out-byte (logand #xff (ash item -8)) outf emit)
    (out-byte (logand #xff item) outf emit))
  2)

(defun out-three (item outf emit)
  (when emit 
    (out-byte (logand #xff (ash item -16)) outf emit)
    (out-byte (logand #xff (ash item -8)) outf emit)
    (out-byte (logand #xff item) outf emit))
  3)

(defun out-long (item outf emit)
  (when emit 
    (out-byte (logand #xff (ash item -24)) outf emit)
    (out-byte (logand #xff (ash item -16)) outf emit)
    (out-byte (logand #xff (ash item -8)) outf emit)
    (out-byte (logand #xff item) outf emit))
  4)

(defun calc-word-size (item)
  "return the size of item as it will be emitted"
  (declare (ignore item))
  2)

(defun out-flexi-num (item outf emit)
  "write the flexinum, return final size; high-bit set means 1-byte, unset two bytes"
  (assert (zerop (logand #x80 (ash item -8))))
  (if nil ;(<= 0 item 127)
      (progn
        (when emit (out-byte (logior #x80 item) outf emit))
        1)
    (progn
      (when emit
        (out-byte (logand #x7f (ash item -8)) outf emit)
        (out-byte (logand #xff item) outf emit))
      2)))

(defun out-byte (item outf emit)
  (when emit
    (put-byte outf (logand #xff item))))

;; temp stuff
(defun string-to-binary (s)
  (let ((r nil) (i 0))
    (loop until (= i (length s)) do
          (let ((c (char s i)) (val #\Space))
            (case c
              (#\\ (incf i)
                   (case (char s i)
                     (#\n (setf val #\Newline) (incf i))
                     (#\b (setf val #\Backspace) (incf i))
                     (#\t (setf val #\Tab) (incf i))
                     (#\f (setf val #\Formfeed) (incf i))
                     (#\r (setf val #\Return) (incf i))
                     ((#\" #\' #\\) (setf val (char s i)) (incf i))
                     (otherwise 
                      (multiple-value-setq (val i)
                          (parse-integer s :start i :radix 8 :junk-allowed t))
                      (setf val (code-char val))))
                   (push val r))

              (otherwise (push c r) (incf i)))))
    (coerce (reverse r) 'string)))

(defun dumpasm ()
  (maphash #'(lambda (key val) (format t "~A : ~A~%" key val)) *labels*))
