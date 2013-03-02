(in-package :salem-layer-util)

(deflayer neg ()
  ;;Coord - cc
  ;;Coord - bc
  ;;Coord - bs
  ;;Coord - sz
  ;;int8  - en
  ;;for i from 0 to en-1
  ;;  int8   - epid
  ;;  uint16 - cn
  ;;  for o from 0 to cn-1
  ;;    Coord - ep(epid)(o)
  (:defdata (buf io off out) ()
    :int16 ("cc(x)" "cc(y)" "bc(x)" "bc(y)" "bs(x)" "bs(y)" "sz(x)" "sz(y)")
    :let en = :int8 "en"
    (loop for i from 0 to (1- en)
       do (progn
            :let epid = :int8 "epid"
            :let cn = :uint16 "cn"
            (loop for o from 0 to (1- cn)
               do :int16 ((format nil "ep(~A)(~A)(x)" epid o) (format nil "ep(~A)(~A)(y)" epid o))))))
  (:defdata-binary (in-file io buffer in) ()
    ;;cc,bc,bs,sz
    (ntimes 8 :int16)
    :let en = :int8
    (ntimes en
      ;;epid
      :int8
      :let cn = :uint16
      (ntimes cn
        ;;ep(epid)(o)
        (ntimes 2 :int16)))))
