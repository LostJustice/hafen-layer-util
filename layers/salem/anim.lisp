(in-package :salem-layer-util)

(deflayer anim ()
  ;;Anim
  ;;int16 - id
  ;;uint16 - d
  ;;uint16 - ids.len
  ;;1 -> ids.len
  ;;  uint16 - ids[i]
  (:defdata (buf io off out) ()
    :int16 "id"
    :uint16 "d"
    :let len = :uint16 "ids-len"
    (dotimes (i len)
      :uint16 ((format nil "ids[~A]" i))))
  (:defdata-binary (in-file io buffer in) ()
    :int16
    :uint16
    :let len = :uint16
    (ntimes len 
      :uint16)))
