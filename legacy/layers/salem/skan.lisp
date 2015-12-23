(in-package :salem-layer-util)

(deflayer skan ()
  ;;ResPose Layer
  ;;uint16 - id
  ;;uint8 - fl
  ;;uint8 - mode
  ;;(mode = 0): WarpMode.ONCE
  ;;(mode = 1): WarpMode.LOOP
  ;;(mode = 2): WarpMode.PONG
  ;;(mode = 3): WarpMode.PONGLOOP
  ;;float - len
  ;;((fl&1) != 0):
  ;;   float - nspeed
  ;;while(off < buf.length):
  ;;  cstring - bnm
  ;;  uint16 - frames.length
  ;;  for 1...frames.length:
  ;;    float - tm
  ;;    floatx3 - trans(1,2,3)
  ;;    float - rang
  ;;    floatx3 - rax(1,2,3)
  (:defdata (buf io off out) ()
    :uint16 "id"
    :let fl = :uint8 "fl"
    :let mode = :uint8 "mode"
    (case mode
      ((0 1 2 3))
      (t (error "skan-layer: invalid mode")))
    :float "len"
    (when (/= (logand fl 1) 0)
      :float "nspeed")
    (while (< off (length buf))
      :cstring "bnm"
      :let frames = :uint16 "frames.length"
      (dotimes (i frames)
        :float ((format nil "tm(~A)" i)
                (format nil "trans(~A)(1)" i)
                (format nil "trans(~A)(2)" i)
                (format nil "trans(~A)(3)" i)
                (format nil "rang(~A)" i)
                (format nil "rax(~A)(1)" i)
                (format nil "rax(~A)(2)" i)
                (format nil "rax(~A)(3)" i)))))
  (:defdata-binary (in-file io buffer in) ()
    :uint16
    :let fl = :uint8
    :let mode = :uint8
    (case mode
      ((0 1 2 3))
      (t (error "skan-layer: invalid mode")))
    :float
    (when (/= (logand fl 1) 0)
      :float "nspeed")
    (do ((bnm (readin-next in)
              (readin-next in)))
        ((eq bnm :eof))
      (stre bnm buffer t)
      :let frames = :uint16
      (ntimes frames
        (ntimes 8 :float)))))
