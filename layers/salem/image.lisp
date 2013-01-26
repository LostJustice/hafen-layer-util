(in-package :salem-layer-util)

(deflayer image ()
  ;;Image Layer
  ;;Produces: #.data - ASCII format
  ;;          #.png  - png image
  ;;Contains: uint16 - z
  ;;          uint16 - subz
  ;;          uint8  - nooff OBSOLETE
  ;;          uint16 - id
  ;;          uint16 - o[x]
  ;;          uint16 - o[y]
  ;;          BUFFER - img : png
  (:defdata (buf io off out) ()
    (write-line ";Image Layer data file" out)
    ;;uint16 z,subz
    (setf off (rw-uints buf off 2 out ";~A [~A]:[unsigned-int-16]:"
                        '(("Z" "z") ("Subz" "subz"))))
    ;;uint8 nooff
    (setf off (rw-uint buf off 1 out ";nooff [noffo]:[unsigned-byte][OBSOLETE]:"))
    ;;uint16 id,o[x],o[y]
    (setf off (rw-uints buf off 2 out ";~A [~A]:[unsigned-int-16]:"
                        '(("ID" "id") ("O[x]" "o") ("O[y]" "o"))))
    ;;BUFFER
    (copy-raw-to-file buf (concatenate 'string io ".png") :start off))
  ;;Image Layer
  ;;Contains: uint16
  ;;          uint16
  ;;          uint8
  ;;          uint16
  ;;          uint16
  ;;          uint16
  ;;          #.png - binary file
  (:defdata-binary (in-file io buffer in) ()
    ;;uint16x2
    (ntimes 2 (rinte in 2 buffer))
    ;;uint8
    (rinte in 1 buffer)
    ;;uint16x3
    (ntimes 3 (rinte in 2 buffer))
    ;;#.png
    (push-file-to-buffer buffer (concatenate 'string in-file ".png"))))
