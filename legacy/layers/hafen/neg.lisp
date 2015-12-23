(in-package :hlu)

(deflayer neg ()
  ;;Neg Layer
  ;;uint16 - cc[x] OFF = 0
  ;;uint16 - cc[y] OFF = 2
  ;;SKIP OFF 4->15
  ;;uint8 - en     OFF = 16
  ;;1 -> en:
  ;;  uint8 - epid
  ;;  uint16 - cn
  ;;  1 -> cn:
  ;;    uint16 - ep[epid][o][x]
  ;;    uint16 - ep[epid][o][y]
  (:defdata (buf io off out) ()
    :uint16 ("cc(x)" "cc(y)")
    (setf off 16)
    :let en = :uint8 "en"
    (dotimes (i en)
      :let epid = :uint8 "epid"
      :let cn = :uint16 "cn"
      (dotimes (j cn)
        :uint16 ((format nil "ep[~A][~A](x)" epid j) 
                 (format nil "ep[~A][~A](y)" epid j)))))
  (:defdata-binary (in-file io buffer in) ()
    :uint16
    :uint16 ;OFF = 4
    (ntimes 12 (vector-push-extend 0 buffer))
    :let en = :uint8
    (ntimes en
      :uint8
      :let cn = :uint16
      (ntimes cn
        :uint16
        :uint16))))
