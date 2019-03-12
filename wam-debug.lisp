(in-package :wam/debug)

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

