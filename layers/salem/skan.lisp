*in-package :salem-layer-util)

(deflayer skan ()
  ;;ResPose Layer
  ;;Produces: #.data - ASCII format
  ;;Contains: uint16 - id
  ;;          uint8  - fl
  ;;          uint8  - mode
  ;;          float  - len
  ;;          if (fl&1)!=0: float  - nspeed
  ;;          loop: String - bnm
  ;;                uint16 - Frames Length
  ;;                loop(^): float - tm
  ;;                         loop(3): float - trans[o]
  ;;                         float - rang
  ;;                         loop(3): float - rax[o]
  (:defdata (buf io off out) ()
    (write-line ";ResPose layer data file" out)
    ;;uint16 id
    (setf off (rw-uint buf off 2 out ";ID [id]:[unsigned-int-16]:"))
    ;;uint8 fl
    (multiple-value-bind (floff fl)
        (rw-uint buf off 1 out ";FL [fn]:[unsigned-bute]")
      ;;uint8 mode
      (format out ";Note: Modes include:0 => ONCE, 1 => LOOP, 2 => PONG, 3 => PONGLOOP")
      (setf floff (rw-uint buf floff 1 out ";Mode [mode]:[unsigned-byte (0-3)]:"))
      ;;float len
      (setf floff (rw-float buf floff out ";Length  [len]:[float]:"))
      ;;float nspeed
      (when (/= (logand fl 1) 0)
        (setf floff (rw-float buf floff out ";NSpeed [nspeed]:[float]:")))
      (do ()
          ((>= floff (length buf)))
        ;;String bnm
        (setf floff (rw-str buf floff out ";String [bname]:"))
        ;;uint16 Frames Length
        (multiple-value-bind (noff flen)
            (rw-uint buf floff 2 out ";Frames Length []:[unsigned-int-16]:")
          (dotimes (i flen)
            ;;floats tm,trans[0-2],rang,rax[0-2]
            (setf noff (rw-floats buf noff out ";~A#~A [~A]:[float]:"
                                  `(("TM" ,i "tm")          ("Trans[0]" ,i "trans")
                                    ("Trans[1]" ,i "trans") ("Trans[2]" ,i "trans")
                                    ("RANG" ,i "rang")      ("Rax[0]" ,i "rax")
                                    ("Rax[1]" ,i "rax")     ("Rax[2]" ,i "rax")))))
          (setf floff noff)))))  
  ;;ResPose Layer
  ;;Contains: uint16
  ;;          uint8 - fl
  ;;          uint8
  ;;          float
  ;;          if (fl&1)!=0: float
  ;;          Loop: String
  ;;                uint16 -fls
  ;;                Loop(fls): float
  ;;                           floatx3
  ;;                           float
  ;;                           floatx3
  (:defdata-binary (in-file io buffer in) ()
    ;;uint16
    (rinte in 2 buffer)
    ;;uint8 - fl
    (let ((fl (readin-int in)))
      (inte fl 1 buffer)
      ;;uint 8
      (rinte in 1 buffer)
      ;;float
      (rfloate in buffer)
      ;;if (fl&1)!=0:
      (when (/= (logand fl 1) 0)
        ;;float
        (rfloate in buffer))
      ;;Loop: String
      (do ((bone (readin-next in)
                 (readin-next in)))
          ((zerop (length bone)))
        (stre bone buffer)
        ;;uint16 - fls
        (let ((fls (readin-int in)))
          (inte fls 2 buffer)
          ;;Loop(fls)
          (ntimes fls
            ;;floatx8
            (ntimes 8 (rfloate in buffer))))))))
