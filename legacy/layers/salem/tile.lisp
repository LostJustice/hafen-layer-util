(in-package :salem-layer-util)

(deflayer tile ()
  ;;Tile Layer
  ;;uint8 - t
  ;;uint8 - id
  ;;uint16 - w
  ;;BUFFER - .png
  (:defdata (buf io off out) ()
    :uint8 ("t" "id")
    :uint16 "w"
    (copy-raw-to-file buf (concatenate 'string io ".png") :start off))
  (:defdata-binary (in-file io buffer in) ()
    :uint8
    :uint8
    :uint16
    (push-file-to-buffer buffer (concatenate 'string in-file ".png"))))
