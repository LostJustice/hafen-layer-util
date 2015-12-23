(in-package :salem-layer-util)

(deflayer tooltip ()
  ;;Tooltip layer
  ;;string - text
  (:defdata (buf io off out) ()
    (write-str ";Tooltip (string):" out)
    (write-sequence buf out)
    (newline out))
  (:defdata-binary (in-file io buffer in) ()
    :string))

;;3/20/13 - :string reads in a newline + string, the newline shouldn't be there...
