(in-package :salem-layer-util)

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
                   (str->ub str)
                   #(0))
      (str->ub str)))

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
