(in-package :salem-layer-util)

(defun remove-cr (str)
  (let ((nstr str))
    (when (and (stringp str)
               (search (ub->str #(13)) str))
      (setf nstr (subseq str 0 (1- (length str)))))
    nstr))

(defun split-neg-opt (opt)
  (let ((com (search "," opt)))
    (cons (parse-integer (subseq opt 0 com))
          (parse-integer (subseq opt (1+ com))))))

(defun get-neg-opts (fd)
  (let ((off :def)
        (sz :def))
    (loop
       do (let ((opt (read-line fd nil :eof)))
            (when (eq opt :eof)
              (return-from get-neg-opts (values (cons off sz) nil)))
            (setf opt (remove-cr opt))
            (if opt
                (cond
                  ((eql (search "off=" opt) 0)
                   (setf off (split-neg-opt (subseq opt 4))))
                  ((eql (search "sz=" opt) 0)
                   (setf sz (split-neg-opt (subseq opt 3))))
                  (t (return-from get-neg-opts (values (cons off sz) opt))))
                (return-from get-neg-opts (values (cons off sz) nil)))))))
    
(defun fix-neg-by-filter (file neg-opts)
  (with-open-file (fd file :element-type '(unsigned-byte 8))
    (when (equalp (str->ub "Haven Resource 1")
                  (read-times fd (length "Haven Resource 1")))
      (with-open-file (nfd (concatenate 'string (namestring file) ".new")
                           :direction :output
                           :element-type '(unsigned-byte 8))
        (write-sequence (str->ub "Haven Resource 1") nfd)
        (write-sequence (read-times fd *version-size*) nfd)
        (loop
             (let ((buf (loop 
                           for ch = (read-byte fd nil :eof)
                           until (or (eq ch :eof)
                                     (= ch 0))
                           do (write-byte ch nfd)
                           collect ch)))
               (when (zerop (length buf))
                 (return))
               (write-byte 0 nfd)
               (let* ((rsz (read-times fd *layer-len-size*))
                      (size (ubarr->uint rsz)))
                 (write-sequence (int->ubarr size 4) nfd)
                 (if (not (equalp buf '(110 101 103)))
                     (write-sequence (read-times fd size) nfd)
                     (let ((neg-data (read-times fd size)))
                       (when (not (eq (car neg-opts) :def))
                         (let ((off-x (int->ubarr (car (car neg-opts)) 2))
                               (off-y (int->ubarr (cdr (car neg-opts)) 2)))
                           (setf (aref neg-data 4) (aref off-x 0))
                           (setf (aref neg-data 5) (aref off-x 1))
                           (setf (aref neg-data 6) (aref off-y 0))
                           (setf (aref neg-data 7) (aref off-y 1))))
                       (when (not (eq (cdr neg-opts) :def))
                         (let ((sz-x (int->ubarr (car (cdr neg-opts)) 2))
                               (sz-y (int->ubarr (cdr (cdr neg-opts)) 2)))
                           (setf (aref neg-data 8) (aref sz-x 0))
                           (setf (aref neg-data 9) (aref sz-x 1))
                           (setf (aref neg-data 10) (aref sz-y 0))
                           (setf (aref neg-data 11) (aref sz-y 1))))
                       (write-sequence neg-data nfd)))))))))
  (delete-file file)
  (rename-file (concatenate 'string (namestring file) ".new")
               file))


(defun s2m (pair)
  (cons (floor (+ (/ (car pair) 4) (/ (cdr pair) 2)))
        (floor (- (/ (cdr pair) 2) (/ (car pair) 4))))) 

(defun fix-neg-by-s2m (file)
  (let ((tmp-fn (concatenate 'string (namestring file) ".tmp")))
    (with-open-file (fd file :element-type '(unsigned-byte 8))
      (when (equalp (str->ub "Haven Resource 1")
                    (read-times fd (length "Haven Resource 1")))
        (with-open-file (nfd tmp-fn 
                             :direction :output 
                             :element-type '(unsigned-byte 8))
          (write-sequence (str->ub "Haven Resource 1") nfd)
          (write-sequence (read-times fd *version-size*) nfd)
        (loop
             (let ((buf (loop 
                           for ch = (read-byte fd nil :eof)
                           until (or (eq ch :eof)
                                     (= ch 0))
                           do (write-byte ch nfd)
                           collect ch)))
               (when (zerop (length buf))
                 (return))
               (write-byte 0 nfd)
               (let* ((rsz (read-times fd *layer-len-size*))
                      (size (ubarr->uint rsz)))
                 (write-sequence (int->ubarr size 4) nfd)
                 (if (not (equalp buf '(110 101 103)))
                     (write-sequence (read-times fd size) nfd)
                     (let* ((neg-data (read-times fd size))
                            (offset-x (make-array 2
                                                :element-type '(unsigned-byte 8)
                                                :displaced-to neg-data 
                                                :displaced-index-offset 4))
                            (offset-y (make-array 2
                                                :element-type '(unsigned-byte 8)
                                                :displaced-to neg-data 
                                                :displaced-index-offset 6))
                            (size-x (make-array 2
                                              :element-type '(unsigned-byte 8)
                                              :displaced-to neg-data 
                                              :displaced-index-offset 8))
                            (size-y (make-array 2
                                              :element-type '(unsigned-byte 8)
                                              :displaced-to neg-data 
                                              :displaced-index-offset 10))
                            (roff-x (ubarr->int offset-x))
                            (roff-y (ubarr->int offset-y))
                            (rsz-x (ubarr->int size-x))
                            (rsz-y (ubarr->int size-y))
                            (new-off (s2m (cons roff-x roff-y)))
                            (new-sz (s2m (cons rsz-x rsz-y)))
                            (new-off-x (int->ubarr (car new-off) 2))
                            (new-off-y (int->ubarr (cdr new-off) 2))
                            (new-sz-x (int->ubarr (car new-sz) 2))
                            (new-sz-y (int->ubarr (cdr new-sz) 2)))
                       (setf (aref neg-data 4) (aref new-off-x 0))
                       (setf (aref neg-data 5) (aref new-off-x 1))
                       (setf (aref neg-data 6) (aref new-off-y 0))
                       (setf (aref neg-data 7) (aref new-off-y 1))
                       (setf (aref neg-data 8) (aref new-sz-x 0))
                       (setf (aref neg-data 9) (aref new-sz-x 1))
                       (setf (aref neg-data 10) (aref new-sz-y 0))
                       (setf (aref neg-data 11) (aref new-sz-y 1))
                       (write-sequence neg-data nfd)))))))))
    (delete-file file)
    (rename-file tmp-fn file)))
   
(defun fix-negs (res-folder filter-file)
  "Fixes the neg layers of select files based on a filter file"
  (let ((name "")
        (next-name nil))
    ;;global run
    (let ((files (solve-files-1 (concatenate 'string res-folder "/"))))
      (dolist (file files)
        (fix-neg-by-s2m file)))
    ;;filter run
    (with-open-file (fd filter-file)
      (while (not (eq name :eof))
        (if next-name
            (setf name next-name)
            (setf name (read-line fd nil :eof)))
        (setf name (remove-cr name))
        (when (stringp name)
          (let ((file (probe-file (concatenate 'string
                                               res-folder
                                               "/"
                                               name
                                               ".res"))))
            (when file
              (multiple-value-bind (opts nn)
                  (get-neg-opts fd)
                (fix-neg-by-filter file opts)
                (setf next-name nn)))))))))
