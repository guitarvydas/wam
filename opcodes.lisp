; $Id: opcodes.lisp,v 1.4 2006/02/18 22:49:02 tarvydas Exp $
;; Copyright 2005 Paul Tarvydas

(in-package :wam)

(defparameter *opcodes* (make-hash-table :test 'equal))

(defun init-opcodes ()
  (clrhash *opcodes*)
  (setf (gethash 'put-x-variable *opcodes*) put-x-variable)
  (setf (gethash 'put-y-variable *opcodes*) put-y-variable)
  (setf (gethash 'put-x-value *opcodes*) put-x-value)
  (setf (gethash 'put-y-value *opcodes*) put-y-value)
  (setf (gethash 'put-y-unsafe-value *opcodes*) put-y-unsafe-value)
  (setf (gethash 'put-structure *opcodes*) put-structure)
  (setf (gethash 'put-list *opcodes*) put-list)
  (setf (gethash 'put-constant *opcodes*) put-constant)
  (setf (gethash 'put-byte-constant *opcodes*) put-byte-constant)
  (setf (gethash 'put-word-constant *opcodes*) put-word-constant)
  (setf (gethash 'put-tri-constant *opcodes*) put-tri-constant)
  (setf (gethash 'put-nil *opcodes*) put-nil)
  (setf (gethash 'get-x-variable *opcodes*) get-x-variable)
  (setf (gethash 'get-y-variable *opcodes*) get-y-variable)
  (setf (gethash 'get-x-value *opcodes*) get-x-value)
  (setf (gethash 'get-y-value *opcodes*) get-y-value)
  (setf (gethash 'get-structure *opcodes*) get-structure)
  (setf (gethash 'get-list *opcodes*) get-list)
  (setf (gethash 'get-constant *opcodes*) get-constant)
  (setf (gethash 'get-byte-constant *opcodes*) get-byte-constant)
  (setf (gethash 'get-word-constant *opcodes*) get-word-constant)
  (setf (gethash 'get-tri-constant *opcodes*) get-tri-constant)
  (setf (gethash 'get-nil *opcodes*) get-nil)
  (setf (gethash 'set-x-variable *opcodes*) set-x-variable)
  (setf (gethash 'set-y-variable *opcodes*) set-y-variable)
  (setf (gethash 'set-x-value *opcodes*) set-x-value)
  (setf (gethash 'set-y-value *opcodes*) set-y-value)
  (setf (gethash 'set-y-value *opcodes*) set-y-value)
  (setf (gethash 'set-constant *opcodes*) set-constant)
  (setf (gethash 'set-byte-constant *opcodes*) set-byte-constant)
  (setf (gethash 'set-word-constant *opcodes*) set-word-constant)
  (setf (gethash 'set-tri-constant *opcodes*) set-tri-constant)
  (setf (gethash 'set-void *opcodes*) set-void)
  (setf (gethash 'unify-x-variable *opcodes*) unify-x-variable)
  (setf (gethash 'unify-x-value *opcodes*) unify-x-value)
  (setf (gethash 'unify-y-value *opcodes*) unify-y-value)
  (setf (gethash 'unify-constant *opcodes*) unify-constant)
  (setf (gethash 'unify-byte-constant *opcodes*) unify-byte-constant)
  (setf (gethash 'unify-word-constant *opcodes*) unify-word-constant)
  (setf (gethash 'unify-tri-constant *opcodes*) unify-tri-constant)
  (setf (gethash 'unify-void *opcodes*) unify-void)
  (setf (gethash 'allocate *opcodes*) allocate)
  (setf (gethash 'deallocate *opcodes*) deallocate)
  (setf (gethash 'call *opcodes*) call)
  (setf (gethash 'execute *opcodes*) execute)
  (setf (gethash 'proceed *opcodes*) proceed)
  (setf (gethash 'try-me-else *opcodes*) try-me-else)
  (setf (gethash 'retry-me-else *opcodes*) retry-me-else)
  (setf (gethash 'trust-me *opcodes*) trust-me)
  (setf (gethash 'try *opcodes*) try)
  (setf (gethash 'retry *opcodes*) retry)
  (setf (gethash 'trust *opcodes*) trust)
  (setf (gethash 'switch-on-term *opcodes*) switch-on-term)
  (setf (gethash 'switch-on-constant *opcodes*) switch-on-constant)
  (setf (gethash 'switch-on-structure *opcodes*) switch-on-structure)
  (setf (gethash 'neck-cut *opcodes*) neck-cut)
  (setf (gethash 'get-level *opcodes*) get-level)
  (setf (gethash 'cut *opcodes*) cut)
  (setf (gethash 'done *opcodes*) done))


(defun disassem (x)
  (case x
    (#.put-x-variable put-x-variable)
    (#.put-y-variable put-y-variable)
    (#.put-x-value put-x-value)
    (#.put-y-value put-y-value)
    (#.put-y-unsafe-value put-y-unsafe-value)
    (#.put-structure put-structure)
    (#.put-list put-list)
    (#.put-constant put-constant)
    (#.put-byte-constant put-byte-constant)
    (#.put-word-constant put-word-constant)
    (#.put-tri-constant put-tri-constant)
    (#.put-nil put-nil)
    (#.get-x-variable get-x-variable)
    (#.get-y-variable get-y-variable)
    (#.get-x-value get-x-value)
    (#.get-y-value get-y-value)
    (#.get-structure get-structure)
    (#.get-list get-list)
    (#.get-constant get-constant)
    (#.get-byte-constant get-byte-constant)
    (#.get-word-constant get-word-constant)
    (#.get-tri-constant get-tri-constant)
    (#.get-nil get-nil)
    (#.set-x-variable set-x-variable)
    (#.set-x-value set-x-value)
    (#.set-y-variable set-y-variable)
    (#.set-y-value set-y-value)
    (#.set-constant set-constant)
    (#.set-byte-constant set-byte-constant)
    (#.set-word-constant set-word-constant)
    (#.set-tri-constant set-tri-constant)
    (#.set-void set-void)
    (#.unify-x-variable unify-x-variable)
    (#.unify-x-value unify-x-value)
    (#.unify-y-value unify-x-value)
    (#.unify-constant unify-constant)
    (#.unify-byte-constant unify-byte-constant)
    (#.unify-word-constant unify-word-constant)
    (#.unify-tri-constant unify-tri-constant)
    (#.unify-void unify-void)
    (#.allocate allocate)
    (#.deallocate deallocate)
    (#.call call)
    (#.execute execute)
    (#.proceed proceed)
    (#.try-me-else try-me-else)
    (#.retry-me-else retry-me-else)
    (#.trust-me trust-me)
    (#.try try)
    (#.retry retry)
    (#.trust trust)
    (#.switch-on-term switch-on-term)
    (#.switch-on-constant switch-on-constant)
    (#.switch-on-structure switch-on-structure)
    (#.neck-cut neck-cut)
    (#.get-level get-level)
    (#.cut cut)
    (#.done done)
    (otherwise nil)))
