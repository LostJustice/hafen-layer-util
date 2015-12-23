(in-package :salem-layer-util)

(deflayer anim ()
  ;;int16  - id
  ;;uint16 - d
  ;;uint16 - ids
  ;;for i from 0 to ids-1
  ;;  uint16 - ids(i)
 (:defdata (buf io off out) ()
   :int16 "id"
   :uint16 "d"
   :let ids = :uint16 "ids"
   (loop for i from 0 to (1- ids)
      do :uint16 ((format nil "ids(~A)" i))))
 (:defdata-binary (in-file io buffer in) ()
   :int16 ;id
   :uint16 ;d
   :let ids = :uint16
   (ntimes ids :uint16))) ;ids(i)
