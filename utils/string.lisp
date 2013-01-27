(in-package :salem-layer-util)


;;;Binary->Folder utils
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

;;;Folder->Binary utils
(defun str->ubarr (str)
  "Convert string into \0 terminated unsigned-byte array"
  ;(declare (optimize (debug 3)))
  (concatenate 'vector
               (string-to-octets str :encoding :utf-8)
               #(0)))

(defun stre (str buffer)
  "Shortcut for encoding a STR and pushing it into the BUFFER"
  ;(declare (optimize (debug 3)))
  (doarr (byte (str->ubarr str))
    (vector-push-extend byte buffer)))

(defun rstre (io buffer)
  "Shortcut for Reading a String from IO, Encoding it and then pushing it onto
the BUFFER"
  (stre (readin-next io) buffer))
