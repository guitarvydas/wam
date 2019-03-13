(in-package wam)

(defconstant put-x-variable 1)
(defconstant put-y-variable 2)
(defconstant put-x-value 3)
(defconstant put-y-value 4)
(defconstant put-y-unsafe-value 5)
(defconstant put-structure 6)
(defconstant put-list 7)
(defconstant put-byte-constant 8)
(defconstant get-x-variable 9)
(defconstant get-y-variable 10)
(defconstant get-x-value 11)
(defconstant get-y-value 12)
(defconstant get-structure 13)
(defconstant get-list 14)
(defconstant get-byte-constant 15)
(defconstant set-x-variable 16)
(defconstant set-y-variable 17)
(defconstant set-x-value 18)
(defconstant set-y-value 19)
(defconstant set-byte-constant 20)
(defconstant set-void 21)
(defconstant unify-x-variable 22)
(defconstant unify-y-variable 23)
(defconstant unify-x-value 24)
(defconstant unify-y-value 25)
(defconstant unify-byte-constant 26)
(defconstant unify-void 27)
(defconstant allocate 28)
(defconstant deallocate 29)
(defconstant call 30)
(defconstant execute 31)
(defconstant proceed 32)
(defconstant try-me-else 33)
(defconstant retry-me-else 34)
(defconstant trust-me 35)
(defconstant try 36)
(defconstant retry 37)
(defconstant trust 38)
(defconstant switch-on-term 39)
(defconstant switch-on-constant 40)
(defconstant switch-on-structure 41)
(defconstant neck-cut 42)
(defconstant get-level 43)
(defconstant cut 44)
(defconstant get-nil 45)
(defconstant put-nil 46)

(defconstant put-word-constant 47)
(defconstant put-tri-constant 48)
(defconstant put-constant 49)

(defconstant get-word-constant 50)
(defconstant get-tri-constant 51)
(defconstant get-constant 52)

(defconstant set-word-constant 53)
(defconstant set-tri-constant 54)
(defconstant set-constant 55)

(defconstant unify-word-constant 56)
(defconstant unify-tri-constant 57)
(defconstant unify-constant 58)


(defconstant done 59)

(defconstant last-opcode 59)


;; defparameter instead of defconstant because of SBCL (see "idiosyncracies" in sbcl manual)
(defparameter opcode-array (make-array 60
                                      :initial-contents '(nil
                                                          f-put-x-variable
                                                          f-put-y-variable
                                                          f-put-x-value
                                                          f-put-y-value
                                                          f-put-y-unsafe-value
                                                          f-put-structure
                                                          f-put-list
                                                          f-put-byte-constant
                                                          f-get-x-variable
                                                          f-get-y-variable
                                                          f-get-x-value
                                                          f-get-y-value
                                                          f-get-structure
                                                          f-get-list
                                                          f-get-byte-constant
                                                          f-set-x-variable
                                                          f-set-y-variable
                                                          f-set-x-value
                                                          f-set-y-value
                                                          f-set-byte-constant
                                                          f-set-void
                                                          f-unify-x-variable
                                                          f-unify-y-variable
                                                          f-unify-x-value
                                                          f-unify-y-value
                                                          f-unify-byte-constant
                                                          f-unify-void
                                                          f-allocate
                                                          f-deallocate
                                                          f-call
                                                          f-execute
                                                          f-proceed
                                                          f-try-me-else
                                                          f-retry-me-else
                                                          f-trust-me
                                                          f-try
                                                          f-retry
                                                          f-trust
                                                          f-switch-on-term
                                                          f-switch-on-constant
                                                          f-switch-on-structure
                                                          f-neck-cut
                                                          f-get-level
                                                          f-cut
                                                          f-get-nil
                                                          f-put-nil
                                                          f-put-word-constant
                                                          f-put-tri-constant
                                                          f-put-constant
                                                          f-get-word-constant
                                                          f-get-tri-constant
                                                          f-get-constant
                                                          f-set-word-constant
                                                          f-set-tri-constant
                                                          f-set-constant
                                                          f-unify-word-constant
                                                          f-unify-tri-constant
                                                          f-unify-constant
                                                          f-done
                                                          )))


