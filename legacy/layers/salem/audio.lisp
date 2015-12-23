(in-package :salem-layer-util)

(deflayer audio ()
  ;;Audio layer
  ;;Produces: #.ogg - binary format
  ;;Contains: BUFFER - raw : ogg
  (:defnone (buf io) ()
    (copy-raw-to-file buf (concatenate 'string io ".ogg")))
  ;;Audio Layer
  ;;Contains: #.ogg, binary file
  (:defnone-binary (in-file io buffer) ()
    (push-file-to-buffer buffer (concatenate 'string in-file ".ogg"))))
