(in-package :salem-layer-util)

(deflayer mesh ()
  ;;Mesh Layer
  ;;uint8  - fl
  ;;uint16 - num
  ;;uint16 - matid
  ;;((fl&2) != 0):
  ;;  uint16 - id
  ;;((fl&4) != 0):
  ;;  uint16 - ref
  ;;((fl&~7) != 0)
  ;;  error
  ;;for 1...(num*3):
  ;;  uint16 - ind[i]
  (:defdata (buf io off out) ()
    :let fl = :uint8 "fl"
    :let num = :uint16 "num"
    :uint16 "matid"
    (when (/= (logand fl 2) 0)
      :uint16 "id")
    (when (/= (logand fl 4) 0)
      :uint16 "ref")
    (when (/= (logand fl (lognot 7)) 0)
      (error (format nil "mesh-layer: invalid fl (~A)~%" fl)))
    (loop 
       for i from 0 to (1- (* num 3))
       do :uint16 ((format nil "ind(~A)" i))))
  (:defdata-binary (in-file io buffer in) ()
    :let fl = :uint8
    :let num = :uint16
    :uint16
    (when (/= (logand fl 2) 0)
      :uint16)
    (when (/= (logand fl 4) 0)
      :uint16)
    (when (/= (logand fl (lognot 7)) 0)
      (error (format nil "mesh-layer: invalid fl (~A)~%" fl)))
    (ntimes (* num 3)
      :uint16)))
