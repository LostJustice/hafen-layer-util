(in-package :salem-layer-util)

(deflayer boneoff ()
  ;;BoneOffset Layer
  ;;Produces: #.data - ASCII format
  ;;Contains: String - name
  ;;          loop: uint8 - opcode
  ;;                if opcode=0: float - x
  ;;                             float - y
  ;;                             float - z
  ;;                if opcode=1: float - angle
  ;;                             float - ax
  ;;                             float - ay
  ;;                             float - az
  ;;                if opcode=2: String- Bone Name
  ;;                if opcode=3: float - rx1
  ;;                             float - ry1
  ;;                             float - rz1
  ;;                             String- Original Name
  ;;                             String- Target Name
  (:defdata (buf io off out) ()
    (write-line ";Bone Offset layer data file" out)
    ;;String name
    (setf off (rw-str buf off out ";String [name]:"))
    (do ()
        ((>= off (length buf)))
      ;;uint8 opcode
      (multiple-value-bind (noff opcode)
          (rw-uint buf off 1 out ";Opcode []:[unsigned-byte (0-3)]:")
        (case opcode
          ;;floats x,y,z
          (0 (setf noff (rw-floats buf noff out ";~A [~A]:[float]:"
                                   '(("X" "x") ("Y" "y") ("Z" "z")))))
          ;;floats angle,ax,ay,az
          (1 (setf noff (rw-floats buf noff out ";~A [~A]:[float]:"
                                   '(("Angle" "ang") ("AX" "ax")
                                     ("AY" "ay") ("AZ" "az")))))
          ;;String bonenm
          (2 (setf noff (rw-str buf noff out ";String [Bone Name]:")))
          ;;floats rx1,ry1,rz1
          (3 (setf noff (rw-floats buf noff out ";~A [~A]:[float]:"
                                   '(("RX1" "rx1") ("RY1" "ry1") ("RZ1" "rz1"))))
             ;;Strings orignm, tgtnm
             (setf noff (rw-strs buf noff out ";String [~A]:"
                                 '("Original Name" "Target Name")))))
        (setf off noff))))
  
  ;;BoneOffset Layer
  ;;Contains: String
  ;;          Loop: uint8 - opcode
  ;;                if opcode=0: floatx3
  ;;                   opcode=1: floatx4
  ;;                   opcode=2: String
  ;;                   opcode=3: floatx3
  ;;                             Stringx2
  (:defdata-binary (in-file io buffer in) ()
    ;;String
    (rstre in buffer)
    ;;Loop: uint8 - opcode
    (do ((opcode (readin-int in)
                 (readin-int in)))
        ((eq opcode :eof))
      (inte opcode 1 buffer)
      (case opcode
        ;;floatx3
        (0 (ntimes 3 (rfloate in buffer)))
        ;;floatx4
        (1 (ntimes 4 (rfloate in buffer)))
        ;;String
        (2 (rstre in buffer))
        ;;floatx3
        (3 (ntimes 3 (rfloate in buffer))
           ;;Stringx2
           (ntimes 2 (rstre in buffer)))
        (t (format t "###ERROR:INVALID OPCODE FOR BONE OFFSET LAYER###~%"))))))
