(in-package :salem-layer-util)

(deflayer codeentry ()
  ;;(string,string) - hash table strings
  (:defdata (buf io off out) ()
    (while (< off (length buf))
      :string ("Key" "Value")))
  (:defdata-binary (in-file io buffer in) ()
    (do ((key (readin-next in)
              (readin-next in)))
        ((equal key ""))
      (stre key buffer)
      :string))) ;value
  
