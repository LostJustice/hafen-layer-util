(in-package :salem-layer-util)

(deflayer action ()
  ;;Action Layer
  ;;cstring - pr
  ;;uint16 - pver
  ;;cstring - name
  ;;cstring - preq
  ;;uint16 - hk
  ;;uint16 - ad.len
  ;;1 -> ad.len:
  ;; cstring - ad[i]
  (:defdata (buf io off out) ()
    :cstring "pr"
    :uint16 "pversion"
    :cstring "name"
    :cstring "prerequisite skill (ignored)"
    :uint16 "hk"
    :let ad = :uint16 "ad length"
    (dotimes (i ad)
      :cstring ((format nil "ad[~A]" i))))
  (:defdata-binary (in-file io buffer in) ()
    :cstring
    :uint16
    :cstring
    :cstring
    :uint16
    :let ad = :uint16
    (ntimes ad
      :cstring)))
