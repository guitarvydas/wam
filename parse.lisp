; $Id: parse.lisp,v 1.7 2006/02/18 22:49:02 tarvydas Exp $
; Copyright 2006 Paul Tarvydas

(defun parse-rule (list)
  (let* ((head (car list))
         (neck (cadr list))
         (body (cddr list))
         (phead (parse-head head))
         (pneck (parse-neck neck))
         (pbody (parse-body body))
         (has-cut (or (contains-cut pneck)
                      (contains-cut pbody))))
    (when has-cut
      (setf (caar phead) 'rule-with-cut))
    `(,@phead
      ,@pneck
      ,@pbody)))

(defun parse-query (list)
  (let ((body (parse-body list)))
    (if (contains-cut body)
        `((query-with-cut ,@body))
      `((query ,@body)))))

(defun contains-cut (list)
  (dolist (i list)
    (when (equal '(cut) i) (return-from contains-cut t)))
  nil)

(defun parse-head (list)
  `((rule ,(car list) ,(1- (length list))
          ,@(mapcar #'parse-arg (cdr list)))))

(defun parse-neck (clause)
  (when clause
    (if (eq clause '!)
        '((neck-cut))
      (list (parse-clause clause)))))

(defun parse-body (list)
  (when list
    (mapcar #'parse-clause list)))

(defun parse-args (list)
  (when list
    `(body ,@(mapcar #'parse-arg list)))) 

(defun parse-arg (item)
  `(,@(cond
       ((varp item)
        `(var ,item))
       ((symbolp item)
        `(const ,item))
       ((stringp item)
        `(const ,item))
       ((numberp item)
        `(const ,item))
       ((vectorp item)
        `(struct ,(aref item 0) ,(1- (length item)) ,@(parse-struct item 1))) 
       ((listp item)
        `(list ,@(mapcar #'parse-arg item))))))

(defun parse-struct (vec index)
  (when (< index (length vec))
    `(,(parse-arg (aref vec index))
      ,@ (parse-struct vec (1+ index))))) 

(defstruct call
  name
  arity
  locals)

(defun parse-clause (clause)
  (if (eq clause '!)
      '(cut)
    `(proc ,(make-call :name (car clause) :arity (1- (length clause)) :locals 0)
           ,@(mapcar #'parse-arg (cdr clause)))))

(defun test0 ()
  (pprint (parse-rule '((father paul albin)))))

(defun test1 ()
  (pprint (parse-rule '((grandfather ?x ?y) (father ?x ?y) (father ?z ?y)))))

(defun test2 ()
  (pprint (parse-rule '((p #(f ?X) #(h ?Y #(f a)) ?Y)))))

(defun test3 ()
  (pprint (parse-rule '((append () ?x ?x)))))

(defun test4 ()
  (pprint (parse-rule '((append (?u  ?x) ?y (?u  ?z))
            (append ?x ?y ?z)))))

(defun test5 ()
  (pprint (parse-query '((p ?Z #(h ?Z ?W) #(f ?W))))))

(defun test6 ()
  (pprint (parse-rule '((p #(f ?X) #(h ?Y #(f a)) ?Y) (father #(y ?X) #(z Y))))))

(defun test7 ()
  (pprint (parse-query '((father ?X ?Y)))))

(defun test8 ()
  (pprint (parse-query '((father ?X ?Y) (father ?Y ?X)))))

(defun test9 ()
  (pprint (parse-query '((p ?Z (?Z ?W) #(f ?W))))))

(defun test10 ()
  (pprint (parse-rule '((bb 1 1 2 3 4)))))

(defun test11 ()
  (pprint (parse-rule '((bb 1 1) ! (bb 2 2)))))

(defun test12 ()
  (pprint (parse-rule '((bb 1 1) (bb 2 2) ! (bb 3 3)))))

(defun test13 ()
  (pprint (parse-rule '((lll (1 2))))))

(defun test14 ()
  (pprint (parse-query '((lll (?X ?Y))))))