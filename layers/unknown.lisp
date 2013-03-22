(in-package :salem-layer-util)

(defun unknown-layer-decode (buf io-name layern)
  (when *verbose*
    (format t "   Layer: ~A~%" io-name)
    (format t "   Len  : ~A~%" (length buf)))
  (let ((nbuf (make-array (+ (length buf) (length layern) 5)
                          :initial-element 0
                          :element-type '(unsigned-byte 8)))
        (off 0))
    (doarr (char (str->ub layern))
      (setf (aref nbuf off) char)
      (incf off))
    (incf off)
    (doarr (int (int->ubarr (length buf) 4))
      (setf (aref nbuf off) int)
      (incf off))
    (doarr (by buf)
      (setf (aref nbuf off) by)
      (incf off))
    (copy-raw-to-file nbuf (concatenate 'string io-name ".unknown"))))

(defun unknown-layer-encode (in-file io)
  (let ((buffer (make-array 1
                            :element-type '(unsigned-byte 8)
                            :adjustable t
                            :fill-pointer 0)))
    (push-file-to-buffer buffer (concatenate 'string in-file ".unknown"))
    (when *verbose*
      (format t "   Layer: ~A~%" in-file)
      (format t "   Len  : ~A~%" (length buffer)))
    (write-sequence (int->ubarr (length buffer) 4) io)
    (write-sequence buffer io)))
