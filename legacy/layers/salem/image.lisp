(in-package :salem-layer-util)

(deflayer image ()
  ;;Image Layer
  ;;uint16 - z
  ;;uint16 - subz
  ;;uint8 - nooff
  ;;uint16 - id
  ;;uint16 - o(x)
  ;;uint16 - o(y)
  ;;BUFFER - .png
  (:defdata (buf io off out) ()
    :uint16 ("z" "subz")
    :uint8 "nooff (obsolete)"
    :uint16 ("id" "o(x)" "o(y)")
    (copy-raw-to-file buf (concatenate 'string io ".png") :start off))
  (:defdata-binary (in-file io buffer in) ()
    :uint16
    :uint16
    :uint8
    :uint16
    :uint16
    :uint16
    (push-file-to-buffer buffer (concatenate 'string in-file ".png"))))
