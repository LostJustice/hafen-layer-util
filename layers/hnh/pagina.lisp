(in-package :salem-layer-util)

(deflayer pagina ()
  ;;string - t
  (:defdata (buf io off out) ()
    (write-line ";Pagina Layer data file" out)
    (write-line ";Text (string):" out)
    (write-line (babel:octets-to-string buf :encoding :utf-8) out))
  (:defdata-binary (in-file io buffer in) ()
    ;;t
    :string))
