(in-package :salem-layer-util)

(deflayer midi ()
  ;;Midi layer [DECODE]
  ;;Produces: #.midi - binary format
  ;;Contains: BUFFER - raw : midi format
  (:defnone (buf io) ()
    (copy-raw-to-file buf (concatenate 'string io ".midi")))
  ;;Midi Layer [ENCODE]
  ;;Contains: #.midi, binary file
  (:defnone-binary (in-file io buffer) ()
    (push-file-to-buffer buffer (concatenate 'string in-file ".midi"))))
