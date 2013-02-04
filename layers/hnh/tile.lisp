(in-package :salem-layer-util)

(deflayer tile ()
  ;;Tile Layer
  ;;uint8 - t
  ;;uint8 - id
  ;;uint16 - w
  ;;.png byte array
  (:defdata (buf io off out) ()
    (write-line ";Tile Layer data file" out)
    :uint8 "t"
    :uint8 "id"
    :uint16 "w"
    (copy-raw-to-file buf (concatenate 'string io ".png") :start off))
  (:defdata-binary (in-file io buffer in) ()
    ;;t, id
    (ntimes 2 :uint8)
    ;;w
    :uint16
    (push-file-to-buffer buffer (concatenate 'string in-file ".png"))))
