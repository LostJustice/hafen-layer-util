(in-package :salem-layer-util)

;;General
(defun ub->str (bytes)
  "Converts a utf-8 encoded byte array to string, general alg comes from babel library"
  (let ((str ""))
    (loop
       for i from 0 to (1- (length bytes))
       do (let ((b0 (aref bytes i)))
            (cond
              ((< b0 #x80)
               (setf str (concatenate 'string str (string (code-char b0)))))
              ((< b0 #xE0)
               (let ((b1 (aref bytes (+ i 1))))
                 (setf str (concatenate 'string
                                        str
                                        (string (code-char
                                                 (logior (ash (logand b0 #x1F) 6)
                                                         (logxor b1 #x80)))))))
               (incf i 1))
              ((< b0 #xf0)
               (let ((b1 (aref bytes (+ i 1)))
                     (b2 (aref bytes (+ i 2))))
                 (setf str (concatenate 'string
                                        str
                                        (string (code-char
                                                 (logior (ash (logand b0 #x0F) 12)
                                                         (ash (logand b1 #x3F) 6)
                                                         (logand b2 #x3f)))))))
               (incf i 2))
              ((< b0 #xF8)
               (let ((b1 (aref bytes (+ i 1)))
                     (b2 (aref bytes (+ i 2)))
                     (b3 (aref bytes (+ i 3))))
                 (setf str (concatenate 'string
                                        str
                                        (string (code-char
                                                 (logior (ash (logand b0 7) 18)
                                                         (ash (logxor b1 #x80) 12)
                                                         (ash (logxor b2 #x80) 6)
                                                         (logxor b3 #x80)))))))
               (incf i 3)))))
    str))
                                                                      

;;;Binary->Folder utils
(defun read-str (buf off)
  (let ((lst ()))
    (do* ((ch (aref buf off)
              (aref buf off)))
         ((zerop ch))
      (setf lst (append lst (cons ch nil)))
      (incf off))
    (values (make-array (length lst)
                        :element-type '(unsigned-byte 8)
                        :initial-contents lst)
            (1+ off))))

(defun rw-str (buf off io fmt)
  "Reads in a string and then writes it to char-IO"
  (multiple-value-bind (str noff)
      (read-str buf off)
    (write-str fmt io)
    (write-sequence str io)
    (newline io)
    (values noff
            (ub->str str))))

(defun rw-strs (buf off io fmt lst)
  "Series of rw-str"
  (dolist (i lst)
    (if (consp i)
        (setf off (rw-str buf off io
                           (apply #'format nil fmt i)))
        (setf off (rw-str buf off io (format nil fmt i)))))
  off)

;;;Folder->Binary utils
(defun str->ubarr (str null-term)
  "Convert string into \0 terminated unsigned-byte array"
  (if null-term
      (concatenate 'vector
                   (string-to-octets str :encoding :utf-8)
                   #(0))
      (string-to-octets str :encoding :utf-8)))

(defun stre (str buffer null-term)
  "Shortcut for encoding a STR and pushing it into the BUFFER"
  (doarr (byte (str->ubarr str null-term))
    (vector-push-extend byte buffer)))


(defun rcstre (io buffer)
  "Shortcut for Reading a String from IO, Encoding it with \0 at end and then pushing 
it onto the BUFFER"
  (stre (readin-next io) buffer t))

(defun rstre (io buffer)
  "Shortcut for Reading a String from IO, Encoding it and then pushing it onto
the BUFFER"
  (stre (readin-next io) buffer nil))
