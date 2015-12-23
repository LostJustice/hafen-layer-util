(in-package :hlu)

(deflayer mat2 (t)
  (lambda (msg fn out)
    (declare (ignore fn))
    (decoder msg out
      :uint16 "id"
      (loop
         until (msg-eom msg)
         do (progn
              :string "nm"
              :dlist))))
  (lambda (fn out in)
    (declare (ignore fn))
    (encoder in out
      :uint16
      (do ((p (next-input in)
              (next-input in)))
          ((null p))
        (estring out p)
        :dlist))))
