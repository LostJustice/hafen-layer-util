(in-package :salem-layer-util)

(deflayer neg ()
  ;;Neg Layer
  ;;Produces: #.data - ASCII format
  ;;Contains: uint16 - cc[x] [off=0]
  ;;          uint16 - cc[y] [off=2]
  ;;          uint8  - en    [off=16]
  ;;          loop(en): uint8 - epid
  ;;                    uint16 - cn (ep[epid] Length)
  ;;                    loop(cn): uint16 - ep[epid][o].[x]
  ;;                              uint16 - ep[epid][o].[y]
  (:defdata (buf io off out) ()
    (write-line ";Neg layer data file" out)
    ;;uint16 cc[x],cc[y]
    (setf off (rw-uints buf off 2 out ";~A [~A]:[unsigned-int-16]:"
                        '(("CC[x]" "cc") ("CC[y]" "cc"))))
    ;;uint8 en
    (multiple-value-bind (boff en)
        (rw-uint buf 16 1 out ";EN [en]:[unsigned-byte]:")
      (1+ boff) ;TODO: get en without boff
      (setf off 17)
      (do ((i 0 (1+ i)))
          ((>= i en))
        ;;uint8 epid
        (setf off (rw-uint buf off 1 out ";EPID [epid]:[unsigned-byte]:"))
        ;;uint16 cn
        (multiple-value-bind (noff cn)
            (rw-uint buf off 2 out ";CN Length [cn]:[unsigned-int-16]:")
          (do ((j 0 (1+ j)))
              ((>= j cn))
            ;;uint16 ep[epid][o].[x],ep[epid][o],[y]
            (setf noff (rw-uints buf noff 2 out ";ep[epid][~A] [~A]:[unsigned-int-16]"
                                 '((j "x") (j "y")))))
          (setf off noff)))))  
  ;;Neg Layer
  ;;Contains: uint16 - OFF[0]
  ;;          uint16 - OFF[2]
  ;;          ignored - OFF[4-15]
  ;;          uint8  - en OFF[16]
  ;;          Loop(en): uint8
  ;;                    uint16 - cn
  ;;                    Loop(cn): uint16
  ;;                              uint16
  (:defdata-binary (in-file io buffer in) ()
    ;;uint16
    (rinte in 2 buffer)
    ;;uint16
    (rinte in 2 buffer) 
    ;;ignore
    (inte 0 12 buffer)
    ;;uint8 - en
    (let ((en (readin-int in)))
      (inte en 1 buffer)
      ;;Loop(en)
      (ntimes en
        ;;uint8
        (rinte in 1 buffer)
        ;;uint16 - cn
        (let ((cn (readin-int in)))
          (inte cn 2 buffer)
          ;;Loop(cn)
          (ntimes cn
            ;;uint16
            (rinte in 2 buffer)
            ;;uint16
            (rinte in 2 buffer)))))))
