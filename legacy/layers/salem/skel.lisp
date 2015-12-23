(in-package :salem-layer-util)

(deflayer skel ()
  ;;Skel Layer
  ;;while(off < buf.length)
  ;;  cstring - bnm
  ;;  floatx3 - pos
  ;;  floatx3 - rax
  ;;  float - rang
  ;;  cstring - bp
  (:defdata (buf io off out) ()
    (while (< off (length buf))
      :cstring "bnm"
      :float ("pos(x)" "pos(y)" "pos(z)"
              "rax(x)" "rax(y)" "rax(z)"
              "rang")
      :cstring "bp"))
  (:defdata-binary (in-file io buffer in) ()
    (do ((bnm (readin-next in)
              (readin-next in)))
        ((eq bnm :eof))
      (stre bnm buffer t)
      (ntimes 7 :float)
      :cstring)))
