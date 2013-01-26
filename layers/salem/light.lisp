(in-package :salem-layer-util)

(deflayer light ()
  ;;Light Layer
  ;;Produces: #.data - ASCII format
  ;;Contains: sint16  - id
  ;;          floatx4 - amb
  ;;          floatx4 - dif
  ;;          floatx4 - spc
  ;;          uint8   - t
  ;;          if t=1: float - ac
  ;;                  float - al
  ;;                  float - aq
  ;;          if t=2: float - x
  ;;                  float - y
  ;;                  float - z
  ;;          if t=3: float - exp
  (:defdata (buf io off out) ()
    (write-line ";Light layer data file" out)
    ;;sint16 id
    (setf off (rw-sint buf off 2 out ";ID [id]:[signed-int-16]:"))
    ;;floatx4 amb
    (setf off (rw-floats buf off out ";Ambient Light [~A] [amb]:[float32]:" 
                         '("r" "g" "b" "a")))
    ;;floatx4 dif
    (setf off (rw-floats buf off out ";Diffuse Light [~A] [dif]:[float32]:"
                         '("r" "g" "b" "a")))
    ;;floatx4 spc
    (setf off (rw-floats buf off out ";Specular Light [~A] [spec]:[float32]:"
                         '("r" "g" "b" "a")))
    ;;uint8 type
    (multiple-value-bind (noff type)
        (rw-uint buf off 1 out ";Type [t]:[unsigned-byte]:")
      (case type
        ;;float ac,al,aq
        (1 (setf noff (rw-floats buf noff out ";~A [~A]:[float32]:"
                                 '(("AC" "ac") ("AL" "al") ("AQ" "aq")))))
        ;;float x,y,z
        (2 (setf noff (rw-floats buf noff out ";~A [~A]:[float32]:"
                                 '(("X" "x") ("Y" "y") ("Z" "z")))))
        ;;float exp
        (3 (setf noff (rw-float buf noff out ";EXP [exp]:[float32]:")))
        ;;invalid layer
        (t (format t "###Error: Light Layer type not 1-3###~%")))))
  
  ;;Light Layer
  ;;Contains: sint16
  ;;          floatx4
  ;;          floatx4
  ;;          floatx4
  ;;          uint8 - t
  ;;          if t=1: floatx3
  ;;             t=2: floatx3
  ;;             t=3: float
  (:defdata-binary (in-file io buffer in) ()
    ;;sint16
    (rinte in 2 buffer)
    ;;floatx12h
    (ntimes 12 (rfloate in buffer))
    ;;uint8 - t
    (let ((ty (readin-int in)))
      (inte ty 1 buffer)
      (case ty
        ;;floatx3
        (1 (ntimes 3 (rfloate in buffer)))
        ;;floatx3
        (2 (ntimes 3 (rfloate in buffer)))
        ;;float
        (3 (rfloate in buffer))
        (t (format t "###ERROR: INVALIDE TYPE FOR LIGHT LAYER###~%"))))))
