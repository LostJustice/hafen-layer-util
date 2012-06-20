(in-package :slayer-util)

;;;General
(defmacro doarr ((var array &optional (result nil)) &body body)
  "dolist, but for arrays"
  (let ((inc (gensym)))
    `(let ((,var 0))
       (do ((,inc 0 (1+ ,inc)))
           ((>= ,inc (length ,array)) ,result)
         (setf ,var (aref ,array ,inc))
         ,@body))))

(defmacro ntimes (n &body body)
  "dotimes without worrying about the variable"
  `(dotimes (,(gensym) ,n)
     ,@body))

(defmacro 2* (num)
  `(* ,num 2))

(defmacro 3* (num)
  `(* ,num 3))

(defmacro while (test-expr &body body)
  `(do ()
       ((not ,test-expr))
     ,@body))


;;;Binary->Folder Input/Output utils
(defun ubarr->uint (ubarr)
  "Makes an array of unsigned bytes into an unsigned integer"
  (reduce #'(lambda (l r)
              (+ (ash l 8) r))
          (nreverse ubarr)))

(defun buf->arr (buf off len)
  "Makes an array out of select slots in the buffer"
  (let ((arr (make-array len
                         :element-type '(unsigned-byte 8))))
    (do ((i 0 (1+ i)))
        ((>= i len))
      ;;transfer
      (setf (aref arr i) (aref buf (+ off i))))
    arr))

(defun read-str (buf off)
  (let ((lst ()))
    (do* ((ch (aref buf off)
              (aref buf off)))
         ((zerop ch))
      (setf lst (append lst (cons ch nil)))
      (incf off))
    (values (babel:octets-to-string (make-array (length lst)
                                                :element-type '(unsigned-byte 8)
                                                :initial-contents lst)
                                    :encoding :utf-8)
            (1+ off))))

(defun rw-str (buf off io fmt)
  "Reads in a string and then writes it to char-IO"
  (multiple-value-bind (str noff)
      (read-str buf off)
    (write-line fmt io)
    (write-line str io)
    (values noff
            str)))

(defun rw-strs (buf off io fmt lst)
  "Series of rw-str"
  (dolist (i lst)
    (if (consp i)
        (setf off (rw-str buf off io
                           (apply #'format nil fmt i)))
        (setf off (rw-str buf off io (format nil fmt i)))))
  off)

(defun read-uint (buf off bytes)
  "Read in an unsigned integer BYTES bytes from BUF@OFF"
  (values (ubarr->uint (buf->arr buf off bytes))
          (+ off bytes)))

(defun read-sint (buf off bytes)
  "Read in a signed integer BYTES bytes from BUF@OFF"
  (multiple-value-bind (val noff)
      (read-uint buf off bytes)
    (if (> val (1- (/ (expt 2 (* 8 bytes)) 2)))
        (values (- val (expt 2 (* 8 bytes))) noff)
        (values val noff))))

(defun rw-uint (buf off bytes io fmt)
  "Read in an unsigned integer and writes it to char-IO"
  (multiple-value-bind (val noff)
      (read-uint buf off bytes)
    (write-line fmt io)
    (write-line (write-to-string val) io)
    (values noff
            val)))

(defun rw-uints (buf off bytes io fmt lst)
  "Series of rw-uint"
  (dolist (i lst)
    (if (consp i)
        (setf off (rw-uint buf off bytes io
                           (apply #'format nil fmt i)))
        (setf off (rw-uint buf off bytes io (format nil fmt i)))))
  off)

(defun rw-sint (buf off bytes io fmt)
  "Read in an signed integer and writes it to char-IO"
  (multiple-value-bind (val noff)
      (read-sint buf off bytes)
    (write-line fmt io)
    (write-line (write-to-string val) io)
    (values noff
            val)))

(defun rw-sints (buf off bytes io fmt lst)
  "Series of rw-sint"
  (dolist (i lst)
    (if (consp i)
        (setf off (rw-sint buf off bytes io
                           (apply #'format nil fmt i)))
        (setf off (rw-sint buf off bytes io (format nil fmt i)))))
  off)

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

(defun write-till (buf io off len)
  "Writes a series of bytes from off->len"
  (setf len (+ len off))
  (do ()
      ((> off len))
    (write-byte (aref buf off) io)
    (incf off)))

;;;;Folder => File util operations
;;;Folder input operations
(defun readin-next (io &optional (limit -1))
  "Reads in our next string input from IO-stream and returns it"
  (let ((buf ""))
    (do ((ln (read-line io nil :eof)
             (read-line io nil :eof))
         (lines 0 (1+ lines)))
        ((eq ln :eof))
      ;;non-comments only
      (when (or (zerop (length ln))
                (char/= (char ln 0) #\;))
        ;;resolve buf
        (if (plusp (length buf))
            (setf buf (concatenate 'string buf (string #\NewLine) ln))
            (setf buf ln))
        
        ;;kill early if next line is comment or EOF or if hit limit
        (when (or (= lines limit) 
                  (eq (peek-char nil io nil :eof) :eof)
                  (char= (peek-char nil io nil :eof) #\;))
          (return-from readin-next buf))))
    buf))

(defun readin-int (io)
  "Reads in an integer from the IO-stream"
  (let ((int (readin-next io 1)))
    (if (zerop (length int))
        :eof
        (parse-integer int))))

(defun readin-float (io)
  "Reads in a float from the IO-stream"
  (let ((float (readin-next io 1)))
    (if (zerop (length float))
        :eof
        (read-from-string float))))

(defun readin-any (io)
  "Reads in the next valid line of any type from IO-stream"
  (read-from-string (readin-next io 1)))

          
;;;To Binary operations
(defun int->ubarr (int bytes)
  "Takes a signed/unsigned INTeger and transforms it into a series of
unsigned bytes given by the BYTES length"
  ;(declare (optimize (debug 3)))
  (let ((ubarr (make-array bytes 
                           :element-type '(unsigned-byte 8)
                           :initial-element 0)))
    (dotimes (i bytes)
      ;;take lower byte out
      (setf (aref ubarr i)
            (logand int #xff))
      ;;push old out rsh
      (setf int (ash int -8)))
    ;;correct order format
    ubarr))

(defun int->buffer (int bytes buffer)
  "Takes a signed/unsinged INTeger and pushes its byte sequence into the buffer"
  (ntimes bytes
    ;;take lower byte out
    (vector-push-extend (logand int #xff) buffer)
    ;;push old out rsh
    (setf int (ash int -8))))

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


(defun str->ubarr (str)
  "Convert string into \0 terminated unsigned-byte array"
  ;(declare (optimize (debug 3)))
  (concatenate 'vector
               (string-to-octets str :encoding :utf-8)
               #(0)))

;;;Binary IO output shortcuts
(defun inte (int bytes buffer)
  "Shortcut for encoding an INT of BYTES-bytes and pushing it into the BUFFER"
  ;(declare (optimize (debug 3)))
  (int->buffer int bytes buffer))

(defun floate (float buffer)
  "Shortcut for encoding a FLOAT and pushing it into the BUFFER"
  ;(declare (optimize (debug 3)))
  (float->buffer float buffer))

(defun stre (str buffer)
  "Shortcut for encoding a STR and pushing it into the BUFFER"
  ;(declare (optimize (debug 3)))
  (doarr (byte (str->ubarr str))
    (vector-push-extend byte buffer)))


(defun rinte (io bytes buffer)
  "Shortcut for Reading an INTeger of BYTES-bytesfrom IO, Encoding it 
and then pushing it onto the BUFFER"
  (inte (readin-int io) bytes buffer))

(defun rfloate (io buffer)
  "Shortcut for Reading a Float from IO, Encoding it and then pushing it onto
the BUFFER"
  (floate (readin-float io) buffer))
  
(defun rstre (io buffer)
  "Shortcut for Reading a String from IO, Encoding it and then pushing it onto
the BUFFER"
  (stre (readin-next io) buffer))


;;;Bianry IO write operations
(defun write-all (buffer io)
  "Writes the entire contents of BUFFER to IO"
  (write-sequence buffer io)
  (doarr (i buffer)
    (write-byte i io)))
