(in-package :salem-layer-util)

(deflayer tooltip ()
  ;;Tooltip Layer
  ;;Produces: #.data - ASCII format
  ;;Contains: String - text
  (:defdata (buf io off out) ()
    (incf off)
    (write-line ";Tooltip layer data file" out)
    ;;string text
    (write-line ";String [text]:" out)
    (write-line (babel:octets-to-string buf :encoding :utf-8) out))
  ;;Tooltip Layer
  ;;Contains: String
  (:defdata-binary (in-file io buffer in) ()
    ;;String
    (rstre in buffer)))
