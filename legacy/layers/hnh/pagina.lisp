(in-package :salem-layer-util)

(deflayer pagina ()
  ;;string - t
  (:defdata (buf io off out) ()
    (write-str ";Text (string):" out)
    (write-sequence buf out)
    (newline out))
  (:defdata-binary (in-file io buffer in) ()
    ;;t
    :string))
