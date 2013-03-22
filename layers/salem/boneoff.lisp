(in-package :salem-layer-util)

(deflayer boneoff ()
  ;;BoneOff Layer
  ;;cstring - nm
  ;;while(off < buf.length):
  ;;  uint8 - opcode
  ;;  (opcode = 0):
  ;;    float - x
  ;;    float - y
  ;;    float - z
  ;;  (opcode = 1):
  ;;    float - ang
  ;;    float - ax
  ;;    float - ay
  ;;    float - az
  ;;  (opcode = 2):
  ;;    string - bonenm
  ;;  (opcode = 3):
  ;;    float - rx1
  ;;    float - ry1
  ;;    float - rz1
  ;;    cstring - orignm
  ;;    cstring - tgtnm
  (:defdata (buf io off out) ()
    :cstring "nm"
    (while (< off (length buf))
      :let op = :uint8 "opcode"
      (case op
        (0 :float ("x" "y" "z"))
        (1 :float ("ang" "ax" "ay" "az"))
        (2 :string "bonenm")
        (3 :float ("rx1" "ry1" "rz1")
           :cstring ("orignm" "tgtnm"))
        (t (error "boneoff-layer: invalid opcode")))))
  (:defdata-binary (in-file io buffer in) ()
    :cstring
    (do ((op (readin-int in)
             (readin-int in)))
        ((eq op :eof))
      (inte op 1 buffer)
      (case op
        (0 (ntimes 3 :float))
        (1 (ntimes 4 :float))
        (2 :cstring)
        (3 (ntimes 3 :float)
           (ntimes 2 :cstring))
        (t (error "boneoff-layer: invalid opcode"))))))
