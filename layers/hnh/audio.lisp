(in-package :salem-layer-util)

(deflayer audio ()
  ;;.ogg - binary ogg
  (:defnone (buf io) ()
    (write-line ";Audio Layer data file")
    (copy-raw-to-file buf (concatenate 'string io ".ogg")))
  (:defnone-binary (in-file io buffer) ()
    (push-file-to-buffer (buffer (concatenate 'string in-file ".ogg")))))
