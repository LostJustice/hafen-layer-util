(in-package :salem-layer-util)

(deflayer codeentry ()
  ;;while off < buf.length:
  ;;  uint8 - t
  ;;  t = 1: while(true):
  ;;           cstring - en
  ;;           cstring - cn
  ;;           en.length() == 0 => break;
  ;;  t = 2: while(true):
  ;;           cstring - ln
  ;;           ln.length() == 0 => break;
  ;;           uint16 - ver
  ;;  else: error
  (:defdata (buf io off out) ()
    (while (< off (length buf))
      :let type = :uint8 "t"
      (case type
        (1 (let ((a t))
             (while a
               :let en = :cstring "en"
               :cstring "cn"
               (when (zerop (length en))
                 (setf a nil)))))
        (2 (let ((a t))
             (while a
               :let ln = :cstring "ln"
               (if (zerop (length ln))
                   (setf a nil)
                   :uint16 "version"))))
        (t (error "codeentry: invalid t")))))
  (:defdata-binary (in-file io buffer in) ()
    (do ((type (readin-int in)
               (readin-int in)))
        ((eq type :eof))
      (inte type 1 buffer)
      (case type
        (1 (let ((a t))
             (while a
               :let en = :cstring
               :cstring
               (when (zerop (length en))
                 (Setf a nil)))))
        (2 (let ((a t))
             (while a
               :let ln = :cstring
               (if (zerop (length ln))
                   (setf a nil)
                   :uint16))))
        (t (error "codeentry: invalid t"))))))
