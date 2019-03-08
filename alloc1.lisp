; $Id: alloc1.lisp,v 1.4 2006/02/08 05:28:12 tarvydas Exp $
; Copyright 2006 Paul Tarvydas

;; rewrite parse tree to include allocation information

;; now each node of kind const/var/list/struct contains
;; an extra item specifying where the item resides - in an
;; X (temporary) register or a Y (stacked) register

;; A (argument) registers are not treated by this allocator
;; since they are trivially decided by the parse - e.g.
;; the first argument always goes in the A1 register, the
;; second into A2, etc.

;; To allow the "environment trimming" optimization,
;; Y registers must be assigned in reverse order
;; of longevity, i.e. the lowest Y registers are
;; assigned to the variables that appear last in
;; rule body.

;; Y registers are assigned to any variable that
;; appears in more than one clause in the body - i.e.
;; variables that appear in more than one clause
;; must be stacked (Y) to preserve their value
;; through the subsequent calls.  All other variables
;; and temporary results are put into X registers (X
;; registers are not stacked and can be overwritten
;; by subsequent calls).

;; For purposes of allocation, the head of a rule is
;; considered to be merged with the first clause of
;; the body.

;; During allocation, we maintain a simplistic symbol
;; table (an alist).  After allocation the parse tree
;; itself contains the resulting information and the
;; symbol table is discarded.

(in-package :wam)

(defun allocate (tree)
  (let ((symbols (nreverse (alloc-top-level tree 0 nil)))
        (has-locals nil))
    (setf has-locals
          (if (is-query tree)
              (mark-all-symbols-as-y symbols)
            (find-y-variables symbols)))
    (when (has-cut tree)
      (setf has-locals t))
    (when symbols
      (let ((y-sorted (sort-by-longevity (extract-y-vars symbols))))
        (assign-y-regs y-sorted (if (has-cut tree) 1 0))))
    (assign-x-regs tree symbols)
    (assign-call-locals tree symbols)
    (values tree symbols has-locals)))

(defun has-cut (tree)
  (or (eq 'rule-with-cut (caar tree))
      (eq 'query-with-cut (caar tree))))

(defstruct alloc
  is-temp
  reg
  first-clause
  last-clause
  is-seen)

(defun make-temp (clause-num)
  (make-alloc :is-temp t :first-clause clause-num :last-clause clause-num
              :is-seen nil))
  
(defun lookup (sym symbol-table)
  (assoc sym symbol-table))

(defun alloc-top-level (tree clause-num symbols)
  (if (null tree)
      symbols
    (alloc-top-level (cdr tree) (1+ clause-num)
                     (alloc (car tree) clause-num symbols nil))))

(defun alloc (tree clause-num symbols nested)
  (if (null tree)
      symbols
    (if (listp (car tree))
        (alloc (cdr tree) clause-num
               (alloc (car tree) clause-num symbols nested) nested)
      (ecase (car tree)
        ((rule rule-with-cut)
         (alloc (cdddr tree) clause-num symbols nested))
        ((query query-with-cut)
         (alloc-top-level (cdr tree) 0 symbols))
        (proc
         (alloc (cddr tree) clause-num symbols nested))
        ((cut neck-cut)
         symbols)
        (struct
         ; create a temp reg to hold the struct, if not at top-level
         (let ((temp (if nested (make-temp clause-num) nil)))
           (when temp
             (push (cons '+anon+ temp) symbols))
           (setf (cdddr tree)
               `(,temp ,@(cdddr tree)))
           (alloc (cddddr tree) clause-num symbols t)))
        (list
         ;; create a temp for every cell of a list
         ;; modify the list so that contains a list of
         ;; pairs - the first of each pair is the cell's temp
         ;; descriptor, the second of each pair is the
         ;; list item, properly allocated
         ;; restricted to [x|y] form, so we're only ever
         ;; working with one cell at a time
         (let ((list-temp (make-temp clause-num)))
           (push (cons '+lanon+ list-temp) symbols)
           ;; now allocate everything inside of the list and
           ;; keep updating the symbols table
           (mapc #'(lambda (item)
                     (let ((new-syms (alloc item clause-num symbols t)))
                       (setf symbols new-syms)))
                 (cdr tree))
           ;; now rearrange the tree so that the first node is
           ;; the list temp, the rest is the list
           (setf (cdr tree)
                 (cons list-temp (cdr tree)))
           symbols))
        (const
         symbols)
        (var
         (let* ((sym (second tree))
                (v (lookup sym symbols))
                (alloc (if v (cdr v) (make-temp clause-num))))
           (when (> clause-num (alloc-last-clause alloc))
             (setf (alloc-last-clause alloc) clause-num))
           (setf (cdr tree)
                 `(,alloc
                   ,(second tree)))
           (if v
               symbols
             (cons (cons sym alloc) symbols))))))))

(defun assign-call-locals (tree symbols)
  (if (is-query tree)
      (let ((nlocals (all-locals symbols)))
        (when (has-cut tree)
          (incf nlocals))
        (loop for clause in (cdar tree)
              do (when (eq 'proc (first clause))
                   (let ((c (second clause)))
                     (setf (call-locals c) nlocals)))))
    (let ((nloc (if (has-cut tree) 1 0)))
      (loop for clause-num from 0 below (length tree)
            for clause in tree
            do (when (eq 'proc  (first clause))
                 (let ((c (second clause)))
                   (setf (call-locals c)
                         (count-locals clause-num symbols nloc))))))))

(defun all-locals (symbols)
  ; return a count of all the local symbols in the symbols list
  (let ((count 0))
    (dolist (sym symbols)
      (let ((alloc (cdr sym)))
        (when (not (alloc-is-temp alloc))
          (incf count))))
    count))
  
(defun count-locals (clause-num symbols start)
  ; return a count of the locals which remain active after
  ; the given clause
  (let ((count start))
    (dolist (sym symbols)
      (let ((alloc (cdr sym)))
        (when (and (not (alloc-is-temp alloc))
                   (< clause-num (alloc-last-clause alloc)))
          (incf count))))
    count))

(defun find-y-variables (symbols)
  ; A Y variable (one that is saved to the stack) is
  ; one whose lifetime spans two or more clauses, where
  ; the rule head (clause #0) is considered to be the same
  ; as the first clause (clause #1)
  ; This routine sets the is-temp flag to nil for every Y variable.
  (mapc #'mark-y symbols)
  (some #'(lambda (pair) (not (alloc-is-temp (cdr pair)))) symbols)) ; return t if any locals

(defun mark-y (var)
  (let* ((alloc (cdr var))
         (first-clause (alloc-first-clause alloc))
         (last-clause (alloc-last-clause alloc)))
    (unless (or (= first-clause last-clause)
                (and (= first-clause 0)
                     (= last-clause 1)))
      (setf (alloc-is-temp alloc) nil))))
             
(defun mark-all-symbols-as-y (symbols)
  (let ((has-locals nil))
  (mapc #'(lambda (var)
            (setf has-locals t)
            (setf (alloc-is-temp (cdr var)) nil)
            (setf (alloc-last-clause (cdr var)) most-positive-fixnum))
        symbols)
  has-locals))
       
(defun is-query (tree)
  (or (eq 'query (caar tree))
      (eq 'query-with-cut (caar tree))))

(defun extract-y-vars (symbols)
  ; return a new list of (pointers to) the y-variables
  (if (null symbols)
      nil
    (if (alloc-is-temp (cdar symbols))
        (extract-y-vars (cdr symbols))
      (cons (car symbols) (extract-y-vars (cdr symbols))))))

(defun sort-by-longevity (y-list)
  (sort y-list #'(lambda (a b)
                   (> (alloc-last-clause (cdr a))
                      (alloc-last-clause (cdr b))))))

(defun assign-y-regs (y-list extra)
  ;; extra is 0 if no cut, 1 if cut (alloc extra y reg for cut reg)
  (loop for reg from 1 upto (length y-list)
        for y in y-list
        do (setf (alloc-reg (cdr y)) (+ reg extra))))

(defun assign-x-regs (tree symbols)
  (let ((reg (if (eq 'query (caar tree))
                 0
               (third (car tree)))))
    (loop for var in symbols
          do (when (alloc-is-temp (cdr var))
               (setf (alloc-reg (cdr var)) (incf reg))))))



(defun atest0 ()
  (multiple-value-bind (tree symbols n-locals)
      (allocate (parse-rule '((father paul albin))))
    (pprint symbols)
    (pprint tree)
    (print n-locals)))

(defun atest1 ()
  (multiple-value-bind (tree symbols n-locals)
      (allocate (parse-rule '((grandfather ?x ?y) (father ?x ?y) (father ?z ?y))))
    (pprint symbols)
    (pprint tree)
    (print n-locals)))

(defun atest2 ()
  (multiple-value-bind (tree symbols n-locals)
      (allocate (parse-rule '((p #(f ?X) #(h ?Y #(f a)) ?Y))))
    (pprint symbols)
    (pprint tree)
    (print n-locals)))

(defun atest3 ()
  (multiple-value-bind (tree symbols n-locals)
      (allocate (parse-rule '((append () ?x ?x))))
    (pprint symbols)
    (pprint tree)
    (print n-locals)))

(defun atest4 ()
  (multiple-value-bind (tree symbols n-locals)
      (allocate (parse-rule '((append (?u . ?x) ?y (?u . ?z))
                              (append ?x ?y ?z))))
    (pprint symbols)
    (pprint tree)
    (print n-locals)))

(defun atest5 ()
  (multiple-value-bind (tree symbols n-locals)
      (allocate (parse-query '((p ?Z #(h ?Z ?W) #(f ?W)))))
    (pprint symbols)
    (pprint tree)
    (print n-locals)))

(defun atest6 ()
  (multiple-value-bind (tree symbols n-locals)
      (allocate (parse-rule '((p #(f ?X) #(h ?Y #(f a)) ?Y) (father #(y ?X) #(z ?Y)))))
    (pprint symbols)
    (pprint tree)
    (print n-locals)))

(defun atest7 ()
  (multiple-value-bind (tree symbols n-locals)
      (allocate (parse-query '((father ?X ?Y))))
    (pprint symbols)
    (pprint tree)
    (print n-locals)))
  
(defun atest8 ()
  (multiple-value-bind (tree symbols n-locals)
      (allocate (parse-query '((father ?X ?Y) (father ?Y ?X))))
    (pprint symbols)
    (pprint tree)
    (print n-locals)))
  
(defun atest9 ()
  (multiple-value-bind (tree symbols n-locals)
      (allocate (parse-rule '((p (?X ?Y)))))
    (pprint symbols)
    (pprint tree)
    (print n-locals)))

(defun atest10 ()
  (multiple-value-bind (tree symbols n-locals)
      (allocate (parse-rule '((p (?X ?Y)) (bb ?X ?Y) ! (bb ?Y ?X))))
    (pprint symbols)
    (pprint tree)
    (print n-locals)))


