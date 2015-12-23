(in-package :salem-layer-util)

(deflayer codeentry ()
  ;;(cstring,cstring) - hash table strings
  (:defdata (buf io off out) ()
    (while (< off (length buf))
      :cstring ("Key" "Value")))
  (:defdata-binary (in-file io buffer in) ()
    (do ((key (readin-next in)
              (readin-next in)))
        ((eq key :eof))
      (stre key buffer t)
      :cstring))) ;value
  
