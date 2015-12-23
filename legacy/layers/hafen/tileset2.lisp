(in-package :hlu)

(deflayer tileset2 ()
  ;;while:
  ;;  uint8 - p
  ;;  p = 0:
  ;;    cstring - tn
  ;;    list - ta
  ;;  p = 1:
  ;;    uint16 - flnum
  ;;    uint16 - flavprob
  ;;    i = 0 -> flnum:
  ;;      cstring - fln
  ;;      uint16  - flv
  ;;      uint8   - flw
  ;;  p = 2:
  ;;    int8 - tag length
  ;;    i = 0 -> tag length:
  ;;      cstring - tag[i]
 (:defdata (buf io off out) ()
   (while (< off (length buf))
     :let p :uint8 "p [0,1,2]"
     (cond
       ((= p 0)
        :cstring "tn"
        :list "ta")
       ((= p 1)
        :let flnum :uint16 "flnum"
        :uint16 "flavprov"
        (dotimes (i flnum)
          :cstring ((format nil "fln(~A)" i))
          :uint16 ((format nil "flv(~A)" i))
          :uint8 ((format nil "flw(~A)" i))))
       ((= p 2)
        :let tag :uint8 "tag length"
        (dotimes (i tag)
          :cstring ((format nil "tag(~A)" i)))))))
 (:defdata-binary (in-file io buffer in) ()
   (do ((p (readin-next in)
           (readin-next in)))
       ((eq p :eof))
     (inte p 1 buffer)
     (cond
       ((= p 0)
        :cstring
        :list)
       ((= p 1)
        :let flnum = :uint16
        :uint16
        (ntimes flnum
          :cstring
          :uint16
          :uint8))
       ((= p 2)
        :let taglen = :uint8
        (ntimes taglen
          :cstring))))))
