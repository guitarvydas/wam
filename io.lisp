; $Id: io.lisp,v 1.1 2006/02/04 06:34:22 tarvydas Exp $
; Copyright 2005 Paul Tarvydas

(defclass io () ())
(defgeneric next-element (x))
(defgeneric reset (x))
(defgeneric put-byte (x b))
(defgeneric tell (x))

(defclass list-io (io)
  ((head :accessor head)
   (current :accessor current)))

(defmethod initialize-instance :after ((obj list-io) &rest init-options &key list
                                       &allow-other-keys)
  (setf (head obj) list)
  (setf (current obj) list))


(defmethod next-element ((x list-io))
  (if (current x)
      (pop (current x))
    nil))

(defmethod reset ((x list-io))
  (setf (current x) (head x)))

(defclass array-io (io)
  ((contents :accessor contents)
   (io-index :accessor io-index)))

(defmethod initialize-instance :after ((obj array-io) &rest init-options &key array
                                       &allow-other-keys)
  (setf (contents obj) array)
  (setf (io-index obj) 0))

(defmethod put-byte ((x array-io) b)
  (setf (aref (contents x) (io-index x)) b)
  (incf (io-index x)))

(defmethod tell ((x array-io))
  (io-index x))

(defmethod reset ((x array-io))
  (setf (io-index x) 0))

