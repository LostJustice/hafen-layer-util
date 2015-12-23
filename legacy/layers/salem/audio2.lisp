(in-package :salem-layer-util)

(deflayer audio2 ()
  ;;Audio2
  ;;uint8 - ver
  ;;ver=1 || ver=2:
  ;;  cstring - id
  ;;  ver = 2:
  ;;    uint16 - bvol
  ;;  BUFFER - ogg type
  ;;else: error
  (:defdata (buf io off out) ()
    :let ver = :uint8 "version"
    :cstring "id"
    (when (= ver 2)
      :uint16 "bvol")
    (copy-raw-to-file buf (concatenate 'string io ".ogg") :start off))
  (:defdata-binary (in-file io buffer in) ()
    :let ver = :uint8
    :cstring
    (when (= ver 2)
      :uint16)
    (push-file-to-buffer buffer (concatenate 'string in-file ".ogg"))))
