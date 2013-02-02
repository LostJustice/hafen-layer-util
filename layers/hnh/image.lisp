(in-package :salem-layer-util)

(deflayer image ()
  ;;Image Layer
  ;;int-16 - z
  ;;int-16 - subz
  ;;int-8  - nooff
  ;;int-16 - id
  ;;int-16 - o (coord=>(int,int))
  ;;.png byte array
  (:defdata (buf io off out) ()
    (decoder (buf off out)
      (write-line ";Image Layer data file" out)
      int-16 "z"
      int-16 "subz"
      int-8 "nooff"
      int-16 "id"
      int-16 "O(x)"
      int-16 "O(y)"
      (copy-raw-to-file buf (concatenate 'string io ".png") :start off)))
  (:defdata-binary (in-file io buffer in) ()
    (encoder (buffer io)
      int-16 ;z 
      int-16 ;subz
      int-8  ;nooff
      int-16 ;id
      int-16 ;o(x)
      int-16 ;o(y)
      (push-file-to-buffer buffer (concatenate 'string in-file ".png")))))
