(in-package :salem-layer-util)

(deflayer pagina ()
  ;;Pagina Layer
  ;;Produces: #.data - ASCII format
  ;;Contains: String - text
  (:defdata (buf io off out) ()
    (incf off) ;;todo: remove
    (write-line ";Pagina layer data file" out)
    ;;string text
    (write-line ";String [text]:" out)
    (write-line (babel:octets-to-string buf :encoding :utf-8) out))
  ;;Pagina Layer
  ;;Contains: String
  (:defdata-binary (in-file io buffer in) ()
    ;;String
    (rstre in buffer)))
