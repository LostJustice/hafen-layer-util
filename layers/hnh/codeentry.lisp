(in-package :salem-layer-util)

(deflayer codeentry ()
  ;;(string,string) - hash table strings
  (:defdata (buf io off out) ()
    (write-line ";CodeEntry Layer data file" out)
    (while (< off (length buf))
      :string ("Key" "Value")))
  (:defdata-binary (in-file io buffer in) ()
    (do ((key (readin-next in)
              (readin-next in)))
        ((eq key :eof))
      (stre key buffer)
      :string))) ;value
  
