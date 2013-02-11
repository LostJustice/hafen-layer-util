(in-package :salem-layer-util)

(deflayer tooltip ()
  ;;Tooltip Layer
  ;;string - t
  (:defdata (buf io off out) ()
    (write-line ";Tooltip Layer data file" out)
    (write-line ";Tooltip (string):" out)
    (write-line (babel:octets-to-string buf :encoding :utf-8) out))
  (:defdata-binary (in-file io buffer in) ()
    ;;t
    :string))
