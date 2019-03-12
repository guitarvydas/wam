(in-package :wam/tags)

; tags
(defconstant int 0) ;integer
(defconstant ref 1) ;reference
(defconstant con 2) ;constant
(defconstant lis 3) ;list
(defconstant str 4) ;structure
(defconstant spcl 5) ; special 0 is nil


(defmacro untag (i)
  `(ash ,i -3))

(defmacro tag (i)
  `(logand ,i 7))

(defmacro tag-int (i)
  `(logior (ash ,i 3) int))

(defmacro tag-ref (i)
  `(logior (ash ,i 3) ref))

(defmacro tag-con (i)
  `(logior (ash ,i 3) con))

(defmacro tag-spcl (i)
  `(logior (ash ,i 3) spcl))

(defmacro tag-lis (i)
  `(logior (ash ,i 3) lis))

(defmacro tag-str (i)
  `(logior (ash ,i 3) str))
