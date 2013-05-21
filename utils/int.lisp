(in-package :salem-layer-util)

;;;Binary->Folder utils
(defun ubarr->uint (ubarr)
  "Makes an array of unsigned bytes into an unsigned integer"
  (reduce #'(lambda (l r)
              (+ (ash l 8) r))
          (nreverse ubarr)))

(defun ubarr->int (ubarr)
  (let ((int (ubarr->uint ubarr)))
    (if (> int (1- (/ (expt 2 (* (length ubarr) 8)) 2)))
        (- int (expt 2 (* 8 (length ubarr))))
        int)))

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
    (write-str fmt io)
    (write-str (write-to-string val) io)
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
    (write-str fmt io)
    (write-str (write-to-string val) io)
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

;;;Folder->Binary utils
(defun readin-int (io)
  "Reads in an integer from the IO-stream"
  (let ((int (readin-next io 1)))
    (if (stringp int)
        (parse-integer int)
        :eof)))


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

;;;Shortcuts Folder->Binary
(defun inte (int bytes buffer)
  "Shortcut for encoding an INT of BYTES-bytes and pushing it into the BUFFER"
  ;(declare (optimize (debug 3)))
  (int->buffer int bytes buffer))

(defun rinte (io bytes buffer)
  "Shortcut for Reading an INTeger of BYTES-bytesfrom IO, Encoding it 
and then pushing it onto the BUFFER"
  (inte (readin-int io) bytes buffer))
