(in-package :hlu)

;;general alg taken from babel to remove dependency on the package


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


(defun str->ub (str)
  "converts a string to bytes"
  (let ((buf (make-array 0 :element-type '(unsigned-byte 8) 
                         :adjustable t
                         :fill-pointer 0)))
    (loop
       for i from 0 to (1- (length str))
       do (let ((ch (char-code (char str i))))
            (cond
              ;;1 byte
              ((< ch #x80)
               (vector-push-extend ch buf))
              ;;2 byte
              ((< ch #x800)
               (vector-push-extend (logior #xC0 (ash ch -6)) buf)
               (vector-push-extend (logior #x80 (logand ch #x3F)) buf))
              ;;3 byte
              ((< ch #x10000)
               (vector-push-extend (logior #xE0 (ash ch -12)) buf)
               (vector-push-extend (logior #x80 (logand #x3F (ash ch -6))) buf)
               (vector-push-extend (logior #x80 (logand #x3F ch)) buf))
              ;;4 byte
              (t
               (vector-push-extend (logior #xf0 (logand #x07 (ash ch -18))) buf)
               (vector-push-extend (logior #x80 (logand #x3F (ash ch -12))) buf)
               (vector-push-extend (logior #x80 (logand #x3F (ash ch -6))) buf)
               (vector-push-extend (logior #x80 (logand ch #x3F)) buf)))))
    buf))
