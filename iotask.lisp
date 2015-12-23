(in-package :hlu)

(defparameter *io-tasks* nil)
(defun read-n (io n)
  (let ((buf (make-array n :element-type `(unsigned-byte 8))))
    (dotimes (i n)
      (setf (aref buf i) (read-byte io nil -1)))
    buf))

(defun io-read-fun ()
  (do ((msg (sb-concurrency:receive-message *io-tasks* :timeout 10)
            (sb-concurrency:receive-message *io-tasks* :timeout 10)))
      ((null msg))
    (with-open-file (fd msg :element-type `(unsigned-byte 8))
      (let ((fsz (file-length fd)))
        (sb-concurrency:send-message *worker-tasks*
                                     `(,(make-msg
                                         :buffer (read-n fd fsz)
                                         :off 0)
                                        . ,msg))))))
