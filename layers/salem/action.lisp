(in-package :salem-layer-util)

(deflayer action ()
  ;;Action Layer
  ;;Produces: #.data - ASCII format
  ;;Contains: String - pr
  ;;          uint16 - pver
  ;;          String - name
  ;;          String - preq
  ;;          uint16 - hk
  ;;          uint16 - ad [Array length]
  ;;          Loop: String - ad
  (:defdata (buf io off out) ()
    (write-line ";Action layer data file" out)
    ;;String pr
    (setf off (rw-str buf off out ";String [pr]:"))
    ;;uint16 pver
    (setf off (rw-uint buf off 2 out ";PVersion [pver]:[unsigned-int-16]:"))
    ;;String name,preq
    (setf off (rw-strs buf off out ";String [~A]:"
                       '("name" "preq:OBSOLETE")))
    ;;uint16 hk
    (setf off (rw-uint buf off 2 out ";HK [hk]:[unsigned-int-16]:"))
    ;;uint16 ad
    (multiple-value-bind (noff ad)
        (rw-uint buf off 2 out ";AD Length [ad]:[unsigned-int-16]:")
      (dotimes (i ad)
        ;;String ad
        (setf noff (rw-str buf noff out (format nil ";String#~A [ad]:" i))))))
  ;;Action Layer
  ;;Contains: String
  ;;          uint16
  ;;          String
  ;;          String
  ;;          uint16
  ;;          uint16 - ad
  ;;          Loop(ad): String
  (:defdata-binary (in-file io buffer in) ()
    ;;String
    (rstre in buffer)
    ;;uint16
    (rinte in 2 buffer)
    ;;String
    (rstre in buffer)
    ;;String
    (rstre in buffer)
    ;;uint16
    (rinte in 2 buffer)
    ;;uint16 - ad
    (let ((ad (readin-int in)))
      (inte ad 2 buffer)
      ;;Loop(ad)
      (ntimes ad
        ;;String
        (rstre in buffer)))))
