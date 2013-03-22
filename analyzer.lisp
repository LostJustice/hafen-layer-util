(in-package :salem-layer-util)

(defun scan-twins(file1 file2)
  "Scans the two files for errors"
  ;(format t "  Scanning...~%")
  (with-open-file (fd1 file1 :element-type '(unsigned-byte 8))
    (with-open-file (fd2 file2 :element-type '(unsigned-byte 8))
      ;;are they the same size?
      (if (= (file-length fd1) (file-length fd2))
          ;;do they have the same elements
          (dotimes (i (file-length fd1))
            (let ((b1 (read-byte fd1 nil :eof))
                  (b2 (read-byte fd2 nil :eof)))
              (when (/= b1 b2)
                ;(return-from scan-twins i))))
                )))
          ;;failed length test
          (return-from scan-twins `(:length ,(file-length fd1) ,(file-length fd2))))))
  nil)

(defun find-twin (file files-2 fold1 fold2 def)
  "Get our twin file"
  ;(format t "  Searching...~%")
  (find file files-2
        :test #'(lambda (x y)
                  (let ((xx (namestring x))
                        (yy (namestring y)))
                    (setf xx
                          (subseq xx (+ (length def) (length fold1))))
                    (setf yy
                          (subseq yy (+ (length def) (length fold2))))))))


(defun analyze (fold1 fold2)
  "Anaylze the contents (.res only) of two folders"
  (let* ((files-1 (car (solve-files fold1 1)))
         (files-2 (car (solve-files fold2 1)))
         (def (namestring *default-pathname-defaults*))
         (len (length files-1))
         (i 0))
    (format t "Processing: ~A files~%" len)
    (dolist (file files-1)
      ;(format t "file ~A/~A :~A ~%" i len (subseq (namestring file)
      ;                                            (+ (length def)
      ;                                               (length fold1))))
      (incf i)
      ;;get it's twin
      (let ((ofile (find-twin file files-2 fold1 fold2 def)))
        (when ofile
          ;;remove it
          (setf files-2 (delete ofile files-2))
          (let ((diff (scan-twins file ofile)))
            (when diff
              ;(format t "  Diff...~%")
              (format t "~A: ~A~%" (subseq (namestring file)
                                           (+ (length def) (length fold1)))
                      diff))))))
      (sleep 0.001)))
