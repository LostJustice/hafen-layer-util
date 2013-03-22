(in-package :salem-layer-util)

(deflayer mat ()
  ;;Mat Layer
  ;;uint16 - id
  ;;while(off < buf.length):
  ;;  cstring - thing
  ;;  (thing = "col"):
  ;;    floatx4 - amb
  ;;    floatx4 - dif
  ;;    floatx4 - spc
  ;;    float - shine
  ;;    floatx4 - emi
  ;;  (thing = "linear")
  ;;  (thing = "mipmap")
  ;;  (thing = "nofacecull")
  ;;  (thing = "tex"):
  ;;    uint16 - id
  ;;  (thing = "texlink"):
  ;;    cstring - nm
  ;;    uint16 - ver
  ;;    uint16 - id
  ;;  (thing = "light"):
  ;;    cstring - l -> pv,pp,vc,pc or n
  (:defdata (buf io off out) ()
    :uint16 "id"
    (while (< off (length buf))
      :let thing = :cstring "thing"
      (setf thing (intern (string-upcase thing) "KEYWORD"))
      (case thing
        (:col :float ("amb(r)" "amb(g)" "amb(b)" "amb(a)"
                      "dif(r)" "dif(g)" "dif(b)" "dif(a)"
                      "spc(r)" "spc(g)" "spc(b)" "spc(a)"
                      "shine"
                      "emi(r)" "emi(g)" "emi(b)" "emi(a)"))
        (:linear)
        (:mipmap)
        (:nofacecull)
        (:tex :uint16 "id")
        (:texlink :cstring "nm"
                  :uint16 ("ver" "id"))
        (:light
         :let l = :cstring "l"
         (setf l (intern (string-upcase l) "KEYWORD"))
         (case l
           ((:pv :pp :vc :pc :n))
           (t (error "mat-layer: invalid l"))))
        (t (error "mat-layer: invalid thing")))))
  (:defdata-binary (in-file io buffer in) ()
    :uint16
    (do ((thing (readin-next in)
                (readin-next in)))
        ((eq thing :eof))
      (stre thing buffer t)
      (setf thing (intern (string-upcase thing) "KEYWORD"))
      (case thing
        (:col (ntimes 17 :float))
        (:linear)
        (:mipmap)
        (:nofacecull)
        (:tex :uint16)
        (:texlink :cstring
                  (ntimes 2 :uint16))
        (:light
         :let l = :cstring "l"
         (setf l (intern (string-upcase l) "KEYWORD"))
         (case l
           ((:pv :pp :vc :pc :n))
           (t (error "mat-layer: invalid l"))))
        (t (error "mat-layer: invalid thing"))))))
