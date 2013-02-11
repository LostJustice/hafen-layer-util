(in-package :salem-layer-util)

(deflayer midi ()
  ;;.midi - binary midi
  (:defnone (buf io) ()
    (write-line ";Midi Layer data file")
    (copy-raw-to-file buf (concatenate 'string io ".midi")))
  (:defnone-binary (in-file io buffer) ()
    (push-file-to-buffer buffer (concatenate 'string in-file ".midi"))))
