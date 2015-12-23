(in-package :hlu)

(deflayer codeentry ()
  ;;while:
  ;; t - uint8
  ;; t=1:
  ;;  (cstring, cstring)
  ;; t=2:
  ;;  resnm - cstring [Dependency code]
  ;;  ver - int
  ;;(cstring,cstring) - hash table strings
  (:defdata (buf io off out) ()
    (while (< off (length buf))
      :let t = :uint8 "t"
      (cond
       ((= t 1)
        :cstring ("Key" "Value"))
       ((= t 2)
        :cstring "Res"
        :uint16 "Version"))))
  (:defdata-binary (in-file io buffer in) ()
    (do ((key (readin-next in)
              (readin-next in)))
        ((eq key :eof))
      (inte key 1 buffer)
      (cond
        ((= key 1)
         :cstring
         :cstring)
        ((= key 2)
         :cstring
         :uint16)))))
