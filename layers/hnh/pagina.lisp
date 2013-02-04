(in-package :salem-layer-util)

(deflayer pagina ()
  ;;string - t
  (:defdata (buf io off out) ()
    (write-line ";Pagina Layer data file" out)
    :string "text")
  (:defdata-binary (in-file io buffer in) ()
    ;;t
    :string))
