(in-package :hlu)

(deflayer vbuf2 (t)
  (lambda (msg fn out)
    (declare (ignore fn))
    (decoder msg out
      :uint8 "fl"
      (let ((num :uint16 "num"))
        (loop
           until (msg-eom msg)
           do (let ((name :string "name"))
                (cond
                  ((string= name "tex")
                   ;;num * 2 size
                   (format out ";;Tex Coordinates~%")
                   (dotimes (i (* num 2))
                     :float32 ((format nil "tex[~A]" i))))
                  ((string= name "nrm")
                   ;;num * 3 size
                   (format out ";;Normal Coordinates~%")
                   (dotimes (i (* num 3))
                     :float32 ((format nil "nrm[~A]" i))))
                  ((string= name "pos")
                   ;;num * 3 size
                   (format out ";;Position Coordinates~%")
                   (dotimes (i (* num 3))
                     :float32 ((format nil "pos[~A]" i))))
                  ((string= name "col")
                   ;;num * 4 size
                   (format out ";;Color Coordinates~%")
                   (dotimes (i (* num 4))
                     :float32 ((format nil "col[~A]" i))))))))))
  (lambda (fn out in)
    (declare (ignore fn out in))
    (error "vbuf2 not defined for encoding")))
                
