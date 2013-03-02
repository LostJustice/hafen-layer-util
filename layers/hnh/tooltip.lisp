(in-package :salem-layer-util)

(deflayer tooltip ()
  ;;Tooltip Layer
  ;;string - t
  (:defdata (buf io off out) ()
    (write-str ";Tooltip (string):" out)
    (write-sequence buf out)
    (newline out))
  (:defdata-binary (in-file io buffer in) ()
    ;;t
    :string))
