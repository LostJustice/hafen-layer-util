(in-package :salem-layer-util)

(deflayer tile ()
  ;;Tile Layer
  ;;Produces: #.data - ASCII format
  ;;          #.png  - png image
  ;;Contains: uint8  - t
  ;;          uint8  - id
  ;;          uint16 - w
  ;;          BUFFER - img : png
  (:defdata (buf io off out) ()
    (write-line ";Tile layer data file" out)
    ;;uint8 t,id
    (setf off (rw-uints buf off 1 out ";~A [~A]:[unsigned-byte]:"
                        '(("T" "t") ("ID" "id"))))
    ;;uint16 w
    (setf off (rw-uint buf off 2 out ";W [w]:[unsigned-int-16]:"))
    ;;BUFFER
    (copy-raw-to-file buf (concatenate 'string io ".png") :start off))
  ;;Tile Layer
  ;;Contains: uint8
  ;;          uint8
  ;;          uint16
  ;;          #.png - binary file
  (:defdata-binary (in-file io buffer in) ()
    ;;uint8
    (rinte in 1 buffer)
    ;;uint8
    (rinte in 1 buffer)
    ;;uint16
    (rinte in 2 buffer)
    ;;#.png
    (push-file-to-buffer buffer (concatenate 'string in-file ".png"))))
