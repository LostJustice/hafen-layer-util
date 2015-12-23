(in-package :salem-layer-util)

(deflayer rlink ()
  ;;RLink Layer
  ;;uint8  - t
  ;;(t = 0):
  ;;  cstring - meshnm
  ;;  uint16 - meshver
  ;;  int16  - meshid
  ;;  cstring - matnm
  ;;  uint16 - matver
  ;;  int16  - matid
  ;;(t = 1):
  ;;  cstring - nm
  ;;  uint16 - ver
  (:defdata (buf io off out) ()
    :let type = :uint8 "t"
    (case type
      (0 :cstring "mesh-nm"
         :uint16 "mesh-ver"
         :int16 "mesh-id"
         :cstring "mat-nm"
         :uint16 "mat-ver"
         :int16 "mat-id")
      (1 :cstring "nm"
         :uint16 "ver")
      (t (error (format nil "rlink-layer: invalid t (~A)->(~A)" type io)))))
  (:defdata-binary (in-file io buffer in) ()
    :let type = :uint8
    (case type
      (0 (ntimes 2
           :cstring
          (ntimes 2 :uint16)))
      (1 :cstring
         :uint16)
      (t (error (format nil "rlink-layer: invalid t (~A)->(~A)" type in-file))))))
