(in-package :salem-layer-util)

(deflayer tileset ()
  ;;uint8  - fl
  ;;uint16 - flnum
  ;;uint16 - flavprob
  ;;for i from 0 to flnum-1 
  ;;  string - fln(i)
  ;;  uint16 - flv(i)
  ;;  uint8  - flw(i)
 (:defdata (buf io off out) ()
    (write-line ";Tileset Layer data file" out)
    :uint8 "fl"
    :let flnum = :uint16 "flnum"
    :uint16 "flavprob"
    (loop for i from 0 to (1- flnum)
       do (progn
            :string ((format nil "fln(~A)" i))
            :uint16 ((format nil "flv(~A)" i))
            :uint8  ((format nil "flw(~A)" i)))))
 (:defdata-binary (in-file io buffer in) ()
   :uint8 ;fl
   :let flnum = :uint16
   :uint16 ;flavprob
   (ntimes flnum
     :string ;fln
     :uint16 ;flv
     :uint8))) ;flw
