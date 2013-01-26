(in-package :salem-layer-util)

(defun copy-raw-to-file (buf filename &key (start 0) (end nil))
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :element-type 'unsigned-byte)
    (write-sequence buf out :start start :end end)))

(defun push-file-to-buffer (buf filename)
  (with-open-file (in filename
                      :direction :input
                      :element-type 'unsigned-byte)
    (let ((seq (make-array (file-length in)
                           :element-type '(unsigned-byte 8))))
      (read-sequence seq in)
      (doarr (byte seq)
        (vector-push-extend byte buf)))))
