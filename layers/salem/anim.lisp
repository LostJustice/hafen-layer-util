(in-package :salem-layer-util)

(deflayer anim ()
  ;;Anim Layer
  ;;Produces: #.data - ASCII format
  ;;Contains: uint16 - id
  ;;          uint16 - d
  ;;          uint16 - ids length
  ;;          loop(ids): uint16 - ids[i]
  (:defdata (buf io off out) ()
    (write-line ";Anime layer data file" out)
    ;;uint16 id,d
    (setf off (rw-uints buf off 2 out ";~A [~A]:[unsigned-int-16]:"
                        '(("ID" "id") ("D" "d"))))
    ;;uint16 ids
    (multiple-value-bind (noff ids)
        (rw-uint buf off 2 out ";IDS Length [ids]:[unsigned-int-16]:")
      (do ((i 0 (1+ i)))
          ((>= i ids))
        ;;uint16 ids[i]
        (setf noff (rw-uint buf noff 2 out (format nil 
                                                   ";IDS#~A [ids]:[unsigned-int-16]:"
                                                   i))))))
  ;;Anim Layer
  ;;Contains: uint16
  ;;          uint16
  ;;          uint16 - ids
  ;;          Loop(ids): uint16
  (:defdata-binary (in-file io buffer in) ()
    ;;uint16
    (rinte in 2 buffer)
    ;;uint16
    (rinte in 2 buffer)
    ;;uint16 - ids
    (let ((ids (readin-int in)))
      (inte ids 2 buffer)
      ;;Loop(ids)
      (ntimes ids
        ;;uint16
        (rinte in 2 buffer)))))
