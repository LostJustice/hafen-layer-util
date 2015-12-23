(in-package :hlu)

(defconstant +t-end+ 0)
(defconstant +t-int+ 1)
(defconstant +t-str+ 2)
(defconstant +t-coord+ 3)
(defconstant +t-uint8+ 4)
(defconstant +t-uint16+ 5)
(defconstant +t-color+ 6)
(defconstant +t-ttol+ 8)
(defconstant +t-int8+ 9)
(defconstant +t-int16+ 10)
(defconstant +t-nil+ 12)
(defconstant +t-bytes+ 14)
(defconstant +t-float32+ 15)
(defconstant +t-float64+ 16)

;;while:
;;  eom: break;
;;  t = :uint8
;;
(defmacro blist (offset buf)
  `(block blist
     (while (< ,offset (length ,buf))
       :let t = :uint8 "t"
       (case t
         (+t-end+ (return-from blist))
         (+t-int+ :int32 "list-int")
         (+t-str+ :cstring "list-str")
         (+t-coord+ :uint16 ("list-coord(x)" "list-coord(y)"))
         (+t-uint8+ :uint8 "list-uint8")
         (+t-uint16+ :uint16 "list-uint16")
         (+t-int8+ :int8 "list-int8")
         (+t-int16+ :int16 "list-int16")
         (+t-color+ :uint8 ("list-r" "list-g" "list-b" "list-a"))
         (+t-ttol+ (list ,offset ,buf))
         (+t-nil+)
         (+t-bytes+
          :let len :uint8 "bytes-length"
          (when (/= (logand len 128) 0)
            :let nlen :int32 "bytes-real-length"
            (setf len nlen))
          (dotimes (i len)
            :uint8 ((format nil "byte[~A]" i))))
         (+t-float32+)
         (+t-float64+)

(defun lisp-decode (buffer off io dhold)

  )
;;
;;
;;
(defun lisp-encode (buffer io dhold)
  )
