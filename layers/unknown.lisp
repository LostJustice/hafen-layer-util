(in-package :salem-layer-util)

(defun unknown-layer-decode (buf io-name)
  (when *verbose*
    (format t "   Layer: ~A~%" io-name)
    (format t "   Len  : ~A~%" (length buf)))
  (copy-raw-to-file buf (concatenate 'string io-name ".unknown")))

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
