(in-package :salem-layer-util)

(deflayer rlink ()
  ;;RLink Layer
  ;;Produces: #.data - ASCII format
  ;;Contains: uint8  - t
  ;;          String - meshnm
  ;;          uint16 - meshver
  ;;          uint16 - meshid
  ;;          String - matnm
  ;;          uint16 - matver
  ;;          uint16 - matid
  (:defdata (buf io off out) ()
    (write-line ";Rlink layer data file" out)
    ;;uint8 t
    (setf off (rw-uint buf off 1 out ";T [t]:[unsigned-byte]:"))
    ;;String meshnm
    (setf off (rw-str buf off out ";String Mesh Name [meshnm]:"))
    ;;uint16 meshver,meshid
    (setf off (rw-uints buf off 2 out ";~A [~A]:[unsigned-int-16]:"
                        '(("Mesh Version" "meshver")
                          ("Mesh ID" "meshid"))))
    ;;String matnm
    (setf off (rw-str buf off out ";String Mat Name [matnm]:"))
    ;;uint16 matver,matid
    (setf off (rw-uints buf off 2 out ";~A [~A]:[unsigned-int-16]:"
                        '(("Mat Version" "matver")
                          ("Mat ID" "matid")))))  
  ;;RLink Layer
  ;;Contains: uint8
  ;;          String
  ;;          uint16
  ;;          uint16
  ;;          String
  ;;          uint16
  ;;          uint16
  (:defdata-binary (in-file io buffer in) ()
    ;;uint8
    (rinte in 1 buffer)
    ;;x2
    (ntimes 2
      ;;String
      (rstre in buffer)
      ;;uint16x2
      (ntimes 2 (rinte in 2 buffer)))))
