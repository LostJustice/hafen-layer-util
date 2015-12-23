(in-package :hlu)

(defparameter *worker-tasks* nil)
(defparameter *expected* 0)
(defparameter *saw* 0)
(defparameter *lock* (sb-thread:make-mutex :name "count-lock"))
(defparameter *header* "Haven Resource 1")

(defun dif-str (base ext)
  (subseq (format nil "~A" ext) (length (format nil "~A" base))))

(defun decode-file (msg base-dir)
  (ensure-directories-exist base-dir)
  ;;check sig
  (let ((sig (bytes msg 16)))
    (setf sig (map 'list #'(lambda (x)
                             (code-char x)) sig))
    (when (not (string= *header* (coerce sig 'string)))
      (return-from decode-file)))
  ;;get version
  (with-open-file (meta (strcat base-dir "meta.ini")
                        :direction :output
                        :if-exists :supersede)
    (let ((ver (uint16 msg)))
      (format meta ";;Version file~%")
      (format meta "~A~%" ver)))
  ;;read layers
  (let ((layer-ind (make-hash-table :test 'equal)))
    (do ((layer (dstring msg) (dstring msg)))
        ((msg-eom msg))
      (let* ((layer-sz (uint32 msg))
             (layer-msg (make-msg
                         :buffer (bytes msg layer-sz)
                         :off 0)))
        (format t "Decode: ~A, ~A~%" layer layer-sz)
        (dparse-layer layer
                      layer-msg
                      (strcat base-dir
                              layer
                              "/"
                              (format nil "~A" (gethash layer layer-ind 0))))
        (incf (gethash layer layer-ind 0)))
      (when (msg-eom msg)
        (return-from decode-file)))))

(defun worker-decode-fun (out-folder in-folder)
  (do ((msg (sb-concurrency:receive-message *worker-tasks* :timeout 5)
            (sb-concurrency:receive-message *worker-tasks* :timeout 5)))
      ((= *saw* *expected*))
    (when msg
      (let ((buf (car msg))
            (fn (cdr msg)))
        (handler-case
            (decode-file buf (strcat out-folder
                                     "/"
                                     (dif-str in-folder fn)
                                     "/"))
          (error (e)
            (format t "~A~%" e)))
        (sb-thread:with-mutex (*lock*)
          (incf *saw*))))))


(defun layer-max (layer)
  (let ((files (directory (make-pathname :directory (pathname-directory layer)
                                         :name :wild
                                         :type :wild)))
        (max 0))
    (dolist (f files)
      (let ((num (parse-integer (pathname-name f) :junk-allowed t)))
        (when (and (not (null num))
                   (> num max))
          (setf max num))))
    max))

(defun encode-file (in-fn out-fn)
  (ensure-directories-exist (subseq out-fn 0 (1- (length out-fn))))
  (with-open-file (out (subseq out-fn 0 (1- (length out-fn)))
                       :direction :output
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
    (write-sequence (map 'list #'char-code *header*) out)
    (with-open-file (meta (strcat in-fn "meta.ini"))
      (encoder meta out
        :uint16))
    ;;get every layer
    (let ((layers (directory (make-pathname
                              :directory (pathname-directory in-fn)
                              :name :wild))))
      (dolist (layer layers)
        (dotimes (i (1+ (layer-max layer)))
          (eparse-layer (car (last (pathname-directory layer)))
                        out
                        (format nil "~A/~A" layer i)))))))

(defun worker-encode-fun (out-folder in-folder)
  (do ((msg (sb-concurrency:receive-message *worker-tasks* :timeout 5)
            (sb-concurrency:receive-message *worker-tasks* :timeout 5)))
      ((= *saw* *expected*))
    (when msg
      (let ((fn msg))
        (handler-case
            (encode-file (format nil "~A" fn)
                         (strcat out-folder
                                 "/"
                                 (dif-str in-folder fn)))
          (error (e)
            (format t "~A~%" e)))
        (sb-thread:with-mutex (*lock*)
          (incf *saw*))))))
