(in-package :salem-layer-util)

(deflayer tileset ()
  ;;Tileset Layer
  ;;Produces: #.data - ASCII format
  ;;Contains: uint8  - fl
  ;;          uint16 - flnum [# of fln/flv/flws]
  ;;          uint16 - flavprob
  ;;          Loop(flnum): String - fln
  ;;                       uint16 - flv
  ;;                       uint8  - flw
  (:defdata (buf io off out) ()
    (write-line ";Tileset layer data file" out)
    ;;uint8 fl
    (setf off (rw-uint buf off 1 out ";FL [fl]:[unsigned-byte]:"))
    ;;uint16 flnum
    (multiple-value-bind (noff flnum)
        (rw-uint buf off 2 out ";FLNum [flnum]:[unsigned-int-16]:")
      ;;uint16 flavprob
      (setf noff (rw-uint buf noff 2 out ";flavprob [flavprob]:[unsigned-int-16]:"))
      (do ((i 0 (1+ i)))
          ((>= i flnum))
        ;;String fln
        (setf noff (rw-str buf off out (format nil ";String#~A [fln]" i)))
        ;;uint16 flv
        (setf noff (rw-uint buf off 2 out (format nil 
                                                  ";flv#~A [flv]:[unsigned-int-16]:"
                                                  i)))
        ;;uint8 flw
        (setf noff (rw-uint buf off 1 out (format nil 
                                                  ";flw#~A [flw]:[unsigned-int-16]:"
                                                  i))))))  
  ;;Tileset Layer
  ;;Contains: uint8 
  ;;          uint16 - flnum
  ;;          uint16
  ;;          Loop(flnum): String
  ;;                       uint16
  ;;                       uint8
  (:defdata-binary (in-file io buffer in) ()
    ;;uint8
    (rinte in 1 buffer)
    ;;uint16 - flnum
    (let ((flnum (readin-int in)))
      (inte flnum 2 buffer)
      ;;uint16
      (rinte in 2 buffer)
      ;;Loop(flnum)
      (ntimes flnum
        ;;String
        (rstre in buffer)
        ;;uint16
        (rinte in 2 buffer)
        ;;uint8
        (rinte in 1 buffer)))))
