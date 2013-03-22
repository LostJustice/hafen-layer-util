(in-package :salem-layer-util)

(deflayer light ()
  ;;Light Layer
  ;;int16   - id
  ;;floatx4 - amb
  ;;floatx4 - dif
  ;;floatx4 - spc
  ;;while(off < buf.length)
  ;;  uint8 - t
  ;;  (t = 1):
  ;;    float - ac
  ;;    float - al
  ;;    float - aq
  ;;  (t = 2)
  ;;    float - x
  ;;    float - y
  ;;    float - z
  ;;  (t = 3)
  ;;    float - exp
  (:defdata (buf io off out) ()
    :int16 "id"
    :float ("amb(r)" "amb(g)" "amb(b)" "amb(a)"
            "dif(r)" "dif(g)" "dif(b)" "dif(a)"
            "spc(r)" "spc(g)" "spc(b)" "spc(a)")
    (while (< off (length buf))
      :let type = :uint8 "t"
      (case type
        (1 :float ("ac" "al" "aq"))
        (2 :float ("x" "y" "z"))
        (3 :float "exp")
        (t (error "light-layer: invalid t")))))
  (:defdata-binary (in-file io buffer in) ()
    :int16
    (ntimes 12 :float)
    (do ((type (readin-int in)
            (readin-int in)))
        ((eq type :eof))
      (inte type 1 buffer)
      (case type
        (1 (ntimes 3 :float))
        (2 (ntimes 3 :float))
        (3 :float)
        (t (error "light-layer: invalid t"))))))
