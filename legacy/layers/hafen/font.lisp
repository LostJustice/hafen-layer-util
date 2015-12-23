(in-package :hlu)

(deflayer font ()
  ;;Font Layer
  ;;uint8 - version
  ;;verison = 1:
  ;;  uint8 - type
  ;;  type = 1:
  ;;    BUFFER -> truetype-font format
  ;;  else: error
  ;;else: error
  (:defdata (buf io off out) ()
    :uint8 ("version [don't change]" "type [don't change]")
    (copy-raw-to-file buf (concatenate 'string io ".font") :start off))
  (:defdata-binary (in-file io buffer in) ()
    ;;ver and type
    (ntimes 2 :uint8)
    (push-file-to-buffer buffer (concatenate 'string in-file ".font"))))
