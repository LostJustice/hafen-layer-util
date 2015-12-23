(in-package :hlu)

(deflayer action ()
  ;;cstring - pr
  ;;uint16 - pver
  ;;cstring - name
  ;;cstring - preq (ignored)
  ;;uint16 - hk
  ;;uint16 - ad
  ;;for i from 0 to ad-1
  ;;  cstring - ad(i)
  (:defdata (buf io off out) ()
    :cstring "pr"
    :uint16 "pver"
    :cstring "name"
    :cstring "preq (ignored)"
    :uint16 "hk"
    :let ad = :uint16 "ad (Don't change ads...)"
    (loop for i from 0 to (1- ad)
       do :cstring ((format nil "ad(~A) [Don't change ads...]" i))))
  (:defdata-binary (in-file io buffer in) ()
    :cstring ;pr 
    :uint16 ;pver
    ;;name,preq
    (ntimes 2 :cstring)
    :uint16 ;hk
    :let ad = :uint16
    (ntimes ad :cstring))) ;ad(i)
