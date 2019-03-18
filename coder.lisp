; $Id: coder.lisp,v 1.5 2006/02/08 05:28:12 tarvydas Exp $
; Copyright 2006 Paul Tarvydas

;; Emit code (to a list) for the given tree

;; Here, we keep track of whether a variable has
;; actually been created - the emitted instructions
;; differ in the cases where the variable has not
;; been created vs. where the variable has already
;; been seen and created.

;; The top level of the coder is the defrel function which
;; defines groups of relations - all with the same name, but
;; possibly with different arities
(in-package :wam)

(defvar *ctable* (make-hash-table))
(defvar *next-label* 0)

(defmacro defrel (name &rest rules)
  `(let ((r-code (defrel-1 ',name ',rules)))
     (when *wam-debug*
       (wam/debug:tprint r-code))
     (assemble (make-instance 'list-io :list r-code) *code-io*)))

(defmacro defrel0 (name &rest rules)
  `(defrel-1 ',name ',rules))

(defmacro defquery (name &body body)
  `(defquery-with-body ',name ',(car body)))

(defun defrel-1 (name rules)
  ;; all similarly-named rules with the same arity
  ;; must be compiled together
  (clrhash *ctable*)
  (let ((arity-list
	 (sort
	  (delete-duplicates
	   (mapcar #'(lambda (rule)
		       (hash-by-arity name rule))
		   rules))
	  #'<)))
    (put-rules-in-original-order)
    (compile-rules-by-arity arity-list name)))

(defun hash-by-arity (name rule)
  (unless (eq name (caar rule))
    (error "functor mismatch in ~A (~A)" name (caar rule)))
  (let ((arity (1- (length (car rule)))))
    (setf (gethash arity *ctable*) (cons rule (gethash arity *ctable*)))
    arity))
    
(defun put-rules-in-original-order ()
  (maphash #'(lambda (k v) (setf (gethash k *ctable*) (nreverse v))) *ctable*))

(defun compile-rules-by-arity (arity-list name)
  (mapcan #'(lambda (i) (compile-arity i name (gethash i *ctable*))) arity-list))

(defun compile-arity (arity name trees)
  ;; if there is more than one rule at the given
  ;; arity, we must compile try/retry jumps around
  ;; the possible choices
  (let ((proc-name (make-arity-label name arity)))
    (case (length trees)
      (0 nil)
      (1
       `(,(lab-def proc-name)
         ,@(code-rule name arity (first trees))
         proceed))
      (2
       (let ((lab (gen-label)))
         `(,(lab-def proc-name)
           try-me-else ,(lab-ref lab)
           ,@(code-rule name arity (first trees))
           proceed
           ,(lab-def lab)
           trust-me
           ,@(code-rule name arity (second trees))
           proceed)))
      (otherwise
       (let ((lab (gen-label)))
         `(,(lab-def proc-name)
           try-me-else ,(lab-ref lab)
           ,@(code-rule name arity (first trees))
           proceed
           ,(lab-def lab)
           ,@(code-retries name arity (butlast (rest trees)))
           trust-me
           ,@(code-rule name arity (car (last trees)))
           proceed))))))

(defun code-retries (name arity trees)
  (when trees
    (let ((lab (gen-label)))
      `(retry-me-else ,(lab-ref lab)
        ,@(code-rule name arity (car trees))
        proceed
        ,(lab-def lab)
        ,@(code-retries name arity (rest trees))))))

(defun code-rule (name arity source-tree)
  (multiple-value-bind (tree symbols has-locals) 
      (allocate (parse-rule source-tree))
    (declare (ignore symbols))
    (let ((head (code-head name arity (first tree)))
          (body (code-body name arity (rest tree)))
          (has-cut (has-cut tree)))
      (if has-locals
          (if has-cut
              `(allocate get-level 1 ,@head ,@body deallocate)
            `(allocate ,@head ,@body deallocate))
        `(,@head ,@body)))))
    

(defun code-head (name arity head)
  ; the basic function of the head code is to
  ; move data from the argument registers (A)
  ; into the appropriate temp (X) and stack (Y)
  ; registers
  (code-1-head name arity 1 (cdddr head) nil))

(defun code-1-head (name arity a-reg list nested)
  (when list
    (let ((item (car list))
          (rest (rest list)))
      (flet ((tail () (code-1-head name arity (1+ a-reg) rest nested)))
        (ecase (car item)
          (var
           (let* ((alloc (second item))
                  (reg (alloc-reg alloc))
                  (is-temp (alloc-is-temp alloc)))
             (prog1
                 (if nested
                     (if (alloc-is-seen alloc)
                         `(,(if is-temp 'unify-x-value 'unify-y-value)
                           ,reg ,@(tail))
                       `(,(if is-temp 'unify-x-variable 'unify-y-variable)
                         ,reg ,@(tail)))
                   (if (alloc-is-seen alloc)
                       `(,(if is-temp 'get-x-value 'get-y-value)
                         ,reg ,a-reg ,@(tail))
                     `(,(if is-temp 'get-x-variable 'get-y-variable)
                       ,reg ,a-reg ,@(tail))))
               (set-seen alloc))))
          (const
           (let ((k (const-kind item)))
             (if nested
                 `(,(choose-k k 'unify-byte-constant 'unify-word-constant
                              'unify-tri-constant 'unify-constant)
                   ,(const-name k item) ,@(tail))
               `(,(choose-k k 'get-byte-constant 'get-word-constant
                            'get-tri-constant 'get-constant)
                 ,(const-name k item) ,a-reg ,@(tail)))))
          (struct
           (let ((struct-name (make-arity-label (second item) (third item))))
             (flet ((struct-tail ()
                      (code-1-head name arity a-reg (nthcdr 4 item) t)))
               (if nested
                   (let ((reg (alloc-reg (fourth item))))
                   `(unify-x-variable ,reg
                     ,@(tail)
                     get-structure ,struct-name ,reg
                     ,@(struct-tail)))
                 `(get-structure ,struct-name ,a-reg
                   ,@(struct-tail)
                   ,@(tail))))))
          (list
           `(get-list ,a-reg
             ,@(code-1-head name arity a-reg (list (third item)) t)
             ,@(code-1-head name arity a-reg
                            (list (or (fourth item)
                                      '(const nil)))
                            t)
             ,@(tail))))))))

(defun code-body (name arity body)
  ; The basic function of the body code is to
  ; match the body of the procedure against
  ; the actual data in the heap.
  ; When calling procedures, the body code
  ; moves data from the registers (X,Y)
  ; into the appropriate argument (A) registers
  ; then calls the procedures.
  ; Returns a list of wam code.
  (code-1-body name arity body nil 1))

(defun code-1-body (name arity body nested a-reg)
  (when body
    (let ((rest (cdr body))
          (item (car body)))
      (flet ((tail () (code-1-body name arity rest nested (1+ a-reg))))
        (ecase (car item)
          (proc
           (let* ((proc (second item))
                  (cname (call-name proc))
                  (carity (call-arity proc))
                  (clocals  (call-locals proc)))
             `(,@(code-1-body name arity (cddr item) nested 1)
               call ,(lab-ref (make-arity-label cname carity)) ,clocals
               ,@(tail))))
          (var
           (let* ((alloc (second item))
                  (reg (alloc-reg alloc))
                  (is-temp (alloc-is-temp alloc)))
             (prog1
                 (if nested
                     (if (alloc-is-seen alloc)
                         `(,(if is-temp 'unify-x-value 'unify-y-value)
                           ,reg ,@(tail))
                       `(,(if is-temp 'unify-x-variable 'unify-y-variable)
                         ,reg ,@(tail)))
                   (if (alloc-is-seen alloc)
                       `(,(if is-temp 'put-x-value 'put-y-value)
                         ,reg ,a-reg ,@(tail))
                     `(,(if is-temp 'put-x-variable 'put-y-variable)
                       ,reg ,a-reg ,@(tail))))
               (set-seen alloc))))               
          (const
           (let ((k (const-kind item)))
           `(,(if nested
                  (choose-k k 'unify-byte-constant 'unify-word-constant
                            'unify-tri-constant 'unify-constant)
                (choose-k k 'put-byte-constant 'put-word-constant
                          'put-tri-constant 'put-constant))
             ,(const-name k item) ,a-reg ,@(tail))))
          (struct
           (let ((struct-name (make-arity-label (second item) (third item))))
             (flet ((struct-tail ()
                      (code-1-body name arity (nthcdr 4 item) t a-reg)))
               (if nested
                   `(unify-variable ,(alloc-reg (fourth item))
                                    ,@(tail)
                                    put-structure ,struct-name ,(alloc-reg (fourth item))
                                    ,@(struct-tail))
                 `(put-structure ,struct-name ,a-reg
                                 ,@(struct-tail)
                                 ,@(tail))))))
          (list
           `(,(if nested 'unify-list 'put-list) ,(alloc-reg (second item))
             ,@(code-1-body name arity (list (third item)) t a-reg)
             ,@(code-1-body name arity
                            (list (or (fourth item)
                                      '(const nil)))
                            t a-reg)
             ,@(tail)))
          (cut
           `(cut 1 ,@(tail)))
          (neck-cut
           `(neck-cut ,@(tail))))))))
           

(defun query-body (name arity body)
  ; The basic function of the query body code is to
  ; create a matchable pattern on the heap,
  ; then to call some rule.
  (query-1-body name arity body nil 1))

(defun query-1-body (name arity body nested a-reg)
  (when body
    (let ((rest (cdr body))
          (item (car body)))
      (flet ((tail () (query-1-body name arity rest nested (1+ a-reg))))
        (ecase (car item)
          (proc
           (let* ((proc (second item))
                  (cname (call-name proc))
                  (carity (call-arity proc))
                  (clocals  (call-locals proc)))
             `(,@(query-1-body name arity (cddr item) nested 1)
               call ,(lab-ref (make-arity-label cname carity)) ,clocals
               ,@(tail))))
          (var
           (let* ((alloc (second item))
                  (reg (alloc-reg alloc))
                  (is-temp (alloc-is-temp alloc)))
             (prog1
                 (if nested
                     (if (alloc-is-seen alloc)
                         `(,(if is-temp 'set-x-value 'set-y-value)
                           ,reg ,@(tail))
                       `(,(if is-temp 'set-x-variable 'set-y-variable)
                         ,reg ,@(tail)))
                   (if (alloc-is-seen alloc)
                       `(,(if is-temp 'put-x-value 'put-y-value)
                         ,reg ,a-reg ,@(tail))
                     `(,(if is-temp 'put-x-variable 'put-y-variable)
                       ,reg ,a-reg ,@(tail))))
               (set-seen alloc))))               
          (const
           (let ((k (const-kind item)))
             (if nested
                 `(,(choose-k k 'set-byte-constant 'set-word-constant
                              'set-tri-constant 'set-constant)
                   ,(const-name k item) ,@(tail))
               `(,(choose-k k 'put-byte-constant 'put-word-constant
                            'put-tri-constant 'put-constant)
                 ,(const-name k item) ,a-reg ,@(tail)))))
          (struct
           (let ((struct-name (make-arity-label (second item) (third item))))
             (flet ((struct-tail ()
                      (query-1-body name arity (nthcdr 4 item) t a-reg)))
               (if nested
                   `(set-variable ,(alloc-reg (fourth item))
                                  ,@(tail)
                                  'put-structure ,struct-name ,(alloc-reg (fourth item))
                                    ,@(struct-tail))
                 `(put-structure ,struct-name ,a-reg
                                 ,@(struct-tail)
                                 ,@(tail))))))
          (list
           `(,(if nested 'set-list 'put-list) ,(alloc-reg (second item))
             ,@(query-1-body name arity (list (third item)) t a-reg)
             ,@(query-1-body name arity
                            (list (or (fourth item)
                                      '(const nil)))
                            t a-reg)
             ,@(tail))))))))

(defun const-kind (item)
  (let ((c (second item)))
    (if (numberp c)
        (cond ((<= -128 c 127) 0)
              ((<= -65536 c 65535) 1)
              (t 2))
      3)))

(defun choose-k (k a b c d)
  (ecase k
    (0 a)
    (1 b)
    (2 c)
    (3 d)))

(defun const-name (k item)
  (let ((c (second item)))
    (ecase k
      (0 c)
      (1 (format nil "%~A" c))
      (2 (format nil "&~A" c))
      (3 (format nil "~A" (second item))))))
  
(defun set-seen (alloc)
  (setf (alloc-is-seen alloc) t))

(defun varp (sym)
  (and (symbolp sym) (char= #\? (char (symbol-name sym) 0))))

(defun gen-label ()
  (format nil "L~A" (incf *next-label*)))

(defun make-arity-label (name arity)
  (format nil "~A/~A" name arity))

(defun lab-def (lab)
  (intern (format nil "$~A" lab)))

(defun lab-ref (lab)
  (intern (format nil "?~A" lab)))


;; a query is a conjunction of goals - just like a body
;; of a rule (without the head)

;; this is just for the defquery macro (test.lisp)
(defun defquery-with-body (name body)
  (let* ((query-name (make-arity-label name 0))
         (tree (car (allocate (parse-query body))))
         (goals (cdr tree)))
    `(,(lab-def query-name)
      allocate
      ,@(query-body name 0 goals)
      done
      deallocate)))
  
(defun defquery% (name alloc-tree)
  (let* ((query-name (make-arity-label name 0))
         (tree (car alloc-tree))
         (goals (cdr tree)))
    `(,(lab-def query-name)
      allocate
      ,@(query-body name 0 goals)
      done
      deallocate)))
  
(defmacro ?- (&body body)
  `(run-query ',body))

(defun run-query (body &optional (display nil))
  (multiple-value-bind (tree symbols has-locals)
       (allocate (parse-query body))
    (let ((q-code (defquery% 'query tree)))
      (when display
        (wam/debug:tprint q-code)
        (terpri))
      (assemble (make-instance 'list-io :list q-code) *code-io*)
      (interp-wam (asm-get-name 'query/0)
                  (remove-if #'(lambda (x) (eq '+lanon+ (car x))) symbols)))))

