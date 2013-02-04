(in-package :salem-layer-util)

(deflayer tooltip ()
  ;;Tooltip Layer
  ;;string - t
  (:defdata (buf io off out) ()
    (write-line ";Tooltip Layer data file" out)
    :string "t")
  (:defdata-binary (in-file io buffer in) ()
    ;;t
    :string))
