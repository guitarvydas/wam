(in-package :wam/test)

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
