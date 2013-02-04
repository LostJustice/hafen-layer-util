(in-package :salem-layer-util)

(deflayer action ()
  ;;string - pr
  ;;uint16 - pver
  ;;string - name
  ;;string - preq (ignored)
  ;;uint16 - hk
  ;;uint16 - ad
  ;;for i from 0 to ad-1
  ;;  string - ad(i)
  (:defdata (buf io off out) ()
    (write-line ";Action Layer data file" out)
    :string "pr"
    :uint16 "pver"
    :string "name"
    :string "preq (ignored)"
    :uint16 "hk"
    :let ad = :uint16 "ad"
    (loop for i from 0 to (1- ad)
       do :string ((format nil "ad(~A)" i))))
  (:defdata-binary (in-file io buffer in) ()
    :string ;pr 
    :uint16 ;pver
    ;;name,preq
    (ntimes 2 :string)
    :uint16 ;hk
    :let ad = :uint16
    (ntimes ad :string))) ;ad(i)
