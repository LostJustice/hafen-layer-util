(in-package :salem-layer-util)

(deflayer pagina ()
  ;;pagina
  ;;string - text
  (:defdata (buf io off out) ()
    (write-str ";Text (string):" out)
    (write-sequence buf out)
    (newline out))
  (:defdata-binary (in-file io buffer in) ()
    :string))
