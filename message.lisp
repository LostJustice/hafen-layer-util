(in-package :hlu)

(defconstant +t-end+     0)
(defconstant +t-int+     1)
(defconstant +t-str+     2)
(defconstant +t-coord+   3)
(defconstant +t-uint8+   4)
(defconstant +t-uint16+  5)
(defconstant +t-color+   6)

(defconstant +t-ttol+    8)
(defconstant +t-int8+    9)
(defconstant +t-int16+   10)

(defconstant +t-nil+     12)
(defconstant +t-uid+     13)
(defconstant +t-bytes+   14)
(defconstant +t-float32+ 15)
(defconstant +t-float64+ 16)


(defstruct msg
  (buffer nil :type array)
  (off nil :type (unsigned-byte 32)))

(defmacro msg-sz (msg)
  `(- (length (msg-buffer ,msg))
      (msg-off ,msg)))

(defmacro msg-eom (msg)
  `(= (length (msg-buffer ,msg)) (msg-off ,msg)))

(defmacro us->s (uint bits)
  (let ((mask-var (gensym))
        (res (gensym)))
    `(let ((,mask-var (ash -1 ,bits))
           (,res ,uint))
       (if (logbitp (1- ,bits) ,res)
           (logior ,res ,mask-var)
           ,res))))

(defun uint8 (msg)
  (let ((ret (aref (msg-buffer msg)
                   (msg-off msg))))
    (incf (msg-off msg))
    ret))

(defun uint16 (msg)
  (let ((off (msg-off msg)))
    (incf (msg-off msg) 2)
    (logior (aref (msg-buffer msg) off)
           (ash (aref (msg-buffer msg) (1+ off)) 8))))

(defun uint32 (msg)
  (let ((off (msg-off msg))
        (buf (msg-buffer msg)))
    (incf (msg-off msg) 4)
    (logior
     (aref buf off)
     (ash (aref buf (1+ off)) 8)
     (ash (aref buf (+ 2 off)) 16)
     (ash (aref buf (+ 3 off)) 24))))

(defun uint64 (msg)
  (let ((off (msg-off msg))
        (buf (msg-buffer msg)))
    (incf (msg-off msg) 8)
    (logior
     (aref buf off)
     (ash (aref buf (1+ off)) 8)
     (ash (aref buf (+ 2 off)) 16)
     (ash (aref buf (+ 3 off)) 24)
     (ash (aref buf (+ 4 off)) 32)
     (ash (aref buf (+ 5 off)) 40)
     (ash (aref buf (+ 6 off)) 48)
     (ash (aref buf (+ 7 off)) 56))))   

(defun int8 (msg)
  (us->s (uint8 msg) 8))

(defun int16 (msg)
  (us->s (uint16 msg) 16))

(defun int32 (msg)
  (us->s (uint32 msg) 32))

(defun int64 (msg)
  (us->s (uint64 msg) 32))

(defun dstring (msg)
  (let ((buf (msg-buffer msg)))
    (let ((ub (make-array
               0
               :element-type `(unsigned-byte 8)
               :adjustable t
               :fill-pointer 0)))
      (loop
         for off from (msg-off msg) to (1- (length buf))
         until (= (aref buf off) 0)
         do (vector-push-extend (aref buf off) ub))
      (incf (msg-off msg) (1+ (length ub)))
      (ub->str ub))))

(defun skip (msg n)
  (incf (msg-off msg) n))

(defun bytes (msg n)
  (let ((ret (make-array
              n
              :element-type `(unsigned-byte 8)
              :displaced-to (msg-buffer msg)
              :displaced-index-offset (msg-off msg))))
    (incf (msg-off msg) n)
    ret))
 
(defun bytes-until (msg stop)
  (let ((off (msg-off msg))
        (buf (msg-buffer msg)))
    (loop
       until (= (aref buf off) stop)
       do (progn (incf off)))
    (make-array (- off (msg-off msg))
                :element-type `(unsigned-byte 8)
                :displaced-to (msg-buffer msg)
                :displaced-index-offset (msg-off msg))))

(defun rest-of-bytes (msg)
  (let ((ret 
         (make-array (msg-sz msg)
                     :element-type '(unsigned-byte 8)
                     :displaced-to (msg-buffer msg)
                     :displaced-index-offset (msg-off msg))))
    (setf (msg-off msg) (length (msg-buffer msg)))
    ret))

(defun coord (msg)
  (list (int16 msg) (int16 msg)))

(defun color (msg)
  (list (uint8 msg)
        (uint8 msg)
        (uint8 msg)
        (uint8 msg)))

(defun float32 (msg)
  (ieee-floats:decode-float32 (uint32 msg)))

(defun float64 (msg)
  (ieee-floats:decode-float64 (uint64 msg)))

(defun cpfloat (msg)
  (let* ((e (int8 msg))
         (ty (uint32 msg))
         (m (logand ty #x7fffffff))
         (s (/= (logand ty #x80000000) 0)))
    (when (= e -128)
      (when (zerop m)
        (return-from cpfloat 0))
      (error "Invalid special float encoding"))
    (let ((v (1+ (/ m 2147483648.0))))
      (when s
        (setf v (- v)))
      (* (expt 2 e) v))))

(defun fcolor (msg)
  (list (cpfloat msg)
        (cpfloat msg)
        (cpfloat msg)
        (cpfloat msg)))

(defun dlist (msg)
  (let ((buf (msg-buffer msg))
        (ret ()))
    (block list-loop
      (loop
         for off from (msg-off msg) to (1- (length buf))
         do (let ((ty (uint8 msg)))
              (cond 
                ((= ty +t-end+)
                 (appendn `(,ty . ,+t-end+) ret)
                 (return-from list-loop))
                ((= ty +t-int+)
                 (appendn `(,ty . ,(int32 msg)) ret))
                ((= ty +t-str+)
                 (appendn `(,ty . ,(dstring msg)) ret))
                ((= ty +t-coord+)
                 (appendn `(,ty . ,(coord msg)) ret))
                ((= ty +t-uint8+)
                 (appendn `(,ty . ,(uint8 msg)) ret))
                ((= ty +t-uint16+)
                 (appendn `(,ty . ,(uint16 msg)) ret))
                ((= ty +t-int8+)
                 (appendn `(,ty . ,(int8 msg)) ret))
                ((= ty +t-int16+)
                 (appendn `(,ty . ,(int16 msg)) ret))
                ((= ty +t-color+)
                 (appendn `(,ty . ,(color msg)) ret))
                ((= ty +t-ttol+)
                 (appendn `(,ty . ,(dlist msg)) ret))
                ((= ty +t-nil+)
                 (appendn `(,ty . nil) ret))
                ((= ty +t-uid+)
                 (appendn `(,ty . ,(int64 msg)) ret))
                ((= ty +t-bytes+)
                 (let ((sz (uint8 msg)))
                   (when (not (zerop (logand sz 128)))
                     (setf sz (int32 msg)))
                   (appendn `(,ty . ,(bytes msg sz)) ret)))
                ((= ty +t-float32+)
                 (appendn `(,ty . ,(float32 msg)) ret))
                ((= ty +t-float64+)
                 (appendn `(,ty . ,(float64 msg)) ret))
                (t (error (format nil "Unknown type in TTO list, ~A" ty)))))))
    ret))
