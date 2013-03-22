(in-package :salem-layer-util)

(deflayer tileset ()
  ;;tileset
  ;;uint8 - fl
  ;;uint16 - flnum
  ;;uint16 - flavprob
  ;;1 -> flnum:
  ;;  cstring - fln[i]
  ;;  uint16 - flv[i]
  ;;  uint8  - flw[i]
  (:defdata  (buf io off out) ()
    :uint8 "fl"
    :let flnum = :uint16 "flnum"
    :uint16 "flavprob"
    (dotimes (i flnum)
      :cstring ((format nil "fln[~A]" i))
      :uint16 ((format nil "flv[~A]" i))
      :uint8 ((format nil "flw[~A]" i))))
  (:defdata-binary (in-file io buffer in) ()
    :uint8
    :let flnum = :uint16
    :uint16
    (ntimes flnum
      :cstring
      :uint16
      :uint8)))
