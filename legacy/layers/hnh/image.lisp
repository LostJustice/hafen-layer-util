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
    :int16 "z"
    :int16 "subz"
    :int8 "nooff"
    :int16 "id"
    :int16 "O(x)"
    :int16 "O(y)"
    ;;                                  |->this 'string fucks the decoder/encoder
    (copy-raw-to-file buf (concatenate 'string io ".png") :start off))
  (:defdata-binary (in-file io buffer in) ()
    ;;z & subz
    (ntimes 2 :int16)
    :int8  ;nooff
    ;;id & o(x) & o(y)
    (ntimes 3 :int16)
    (push-file-to-buffer buffer (concatenate 'string in-file ".png"))))
