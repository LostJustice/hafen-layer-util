(in-package :salem-layer-util)

;;;Binary->Folder Input/Output utils
;;Float Format 5-bytes
;;1st byte => exponent for power of 2
;;2-5 byte => uint32
;;sign-bit => 31st bit
;;integer repre => 0-30bit
(defun read-float (buf off)
  "Reads in a 5-byte float from BUF@OFF"
  (multiple-value-bind (e eoff)
      (read-sint buf off 1)
    (multiple-value-bind (u32 noff)
        (read-uint buf eoff 4)
      (let ((m (logand u32 #x7fffffff))
            (s (logand u32 #x80000000)))
        (if (= e -128)
            (if (zerop m)
                (values 0 noff)
                (progn (format t "###ERROR: Decoding Float###~%")
                       (return-from read-float (values -111 noff))))
            (let ((v (1+ (/ m 2147483648.0d0))))
              (if (/= s 0)
                  (values (* (expt 2.0d0 e) (- v)) noff)
                  (values (* (expt 2.0d0 e) v) noff))))))))


(defun rw-float (buf off io fmt)
  "Reads in a 33-bit float from BUF@OFF and streams to IO-stream"
  (multiple-value-bind (f33 noff)
      (read-float buf off)
    (when (= f33 -111)
      (print fmt)
      (print off) (terpri))
    (write-line fmt io)
    (write-line (write-to-string f33) io)
    (values noff
            f33)))

(defun rw-floats (buf off io fmt lst)
  "Series of rw-floats.."
  (dolist (i lst)
    (if (consp i)
        (setf off (rw-float buf off io 
                            (apply #'format nil fmt i)))
        (setf off (rw-float buf off io (format nil fmt i)))))
  off)



;;;Folder->Binary utils
(defun readin-float (io)
  "Reads in a float from the IO-stream"
  (let ((float (readin-next io 1)))
    (if (zerop (length float))
        :eof
        (read-from-string float))))

;;Surely there's a better way to do this...
;;Note: Float accuracy is not very good in the .0000######
;;Note: 0.0d0 for loftar is #(128 0 0 0 0), but for my files it'll be:
;;      #(128 0 0 0 128).
(defun float->ubarr (float)
  "Takes a FLOAT and transforms it into a series of unsigned bytes"
  ;(declare (optimize (debug 3)))
  (cond ((zerop float) (make-array 5 :element-type '(unsigned-byte 8)
                                   :initial-contents #(128 0 0 0 0)))
        (t 
         (let ((sign #x00000000))
           ;;setup sign bit
           (when (minusp float) 
             (setf sign #x80000000)
             (setf float (* float -1)))
           ;;get brute-force p-o-2 exponent till we get good match
           (do ((exp -127 (1+ exp)))
               ((= exp 12))
             ;;get m
             (let ((m (round (* (1- (/ float (expt 2.0d0 exp))) 2147483648.0d0))))
               ;;is it valid match
               (when (and (or (plusp m)
                              (zerop m))
                          (<= m #x7fffffff))
                 ;;make our array
                 (let ((ubarr (make-array 5 
                                          :element-type '(unsigned-byte 8))))
                   ;;stuff it in
                   (setf (aref ubarr 0) (logand exp #xff))
                   (let ((i 1))
                     (doarr (ith (int->ubarr (logior m sign) 4))
                       (setf (aref ubarr i) ith)
                       (incf i)))
                   (return-from float->ubarr ubarr))))))
         (format t "###ERROR: ENCODING FLOAT FAILED"))))



(defun float->buffer (float buffer)
  "Takes a FLOAT and pushes it into the buffer"
  (cond ((zerop float) ;Float 0.0d0
         (vector-push-extend 128 buffer)
         (ntimes 4 
           (vector-push-extend 0 buffer))
         (return-from float->buffer nil))
        (t
         (let ((sign #x00000000))
           ;;setup sign bit
           (when (minusp float)
             (setf sign #x80000000)
             (setf float (* float -1)))
           ;;brute-froce p-o-2 exponent till we get good match
           (do ((exp 0 (1+ exp)))
               ((= exp 127))
             ;;test left
             (let ((m (round (* (1- (/ float (expt 2.0d0 exp))) 2147483648.0d0))))
               ;;validate
               (when (and (or (plusp m)
                              (zerop m))
                          (<= m #x7fffffff))
                 ;;push into buffer
                 (vector-push-extend exp buffer)
                 (inte (logior m sign) 4 buffer)
                 (return-from float->buffer nil)))
             ;;test right
             (let ((m (round (* (1- (/ float (expt 2.0d0 (- exp)))) 
                                2147483648.0d0))))
               ;;validate
               (when (and (or (plusp m)
                              (zerop m))
                          (<= m #x7fffffff))
                 ;;push into buffer
                 (vector-push-extend (logand (- exp) #xff) buffer)
                 (inte (logior m sign) 4 buffer)
                 (return-from float->buffer nil)))))))
  (format t "###ERROR: ENCODING FLOAT FAILED ~A~%" float))


;;;Shortcuts for Folder->Binary
(defun floate (float buffer)
  "Shortcut for encoding a FLOAT and pushing it into the BUFFER"
  ;(declare (optimize (debug 3)))
  (float->buffer float buffer))

(defun rfloate (io buffer)
  "Shortcut for Reading a Float from IO, Encoding it and then pushing it onto
the BUFFER"
  (floate (readin-float io) buffer))
