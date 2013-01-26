(in-package :salem-layer-util)

(deflayer skel ()
  ;;Skeleton Layer
  ;;Produces: #.data - ASCII format
  ;;Contains: loop: String - bnm
  ;;                float  - pos[x]
  ;;                float  - pos[y]
  ;;                float  - pos[z]
  ;;                float  - rax[x]
  ;;                float  - rax[y]
  ;;                float  - rax[z]
  ;;                float  - rang
  ;;                String - bp
  (:defdata (buf io off out) ()
    (write-line ";Skeleton layer data file" out)
    (do ()
        ((>= off (length buf)))
      ;;String bnm
      (setf off (rw-str buf off out ";String [bname]:"))
      ;;floats pos[x,y,z],rax[x,y,z]
      (setf off (rw-floats buf off out ";~A [~A]:[float]:"
                           '(("Pos[x]" "pos") ("Pos[y]" "pos")
                             ("Pos[z]" "pos") ("Rax[x]" "rax")
                             ("Rax[y]" "rax") ("Rax[z]" "rax")
                             ("RANG" "rang"))))
      ;;String bp
      (setf off (rw-str buf off out ";String [bp]:"))))	
  ;;Skeleton Layer
  ;;Contains: Loop: String
  ;;                floatx7
  ;;                String
  (:defdata-binary (in-file io buffer in) ()
    ;;Loop: String
    (do ((name (readin-next in)
               (readin-next in)))
        ((zerop (length name)))
      (stre name buffer)
      ;;floatx7
      (ntimes 7 (rfloate in buffer))
      ;;String
      (rstre in buffer))))
