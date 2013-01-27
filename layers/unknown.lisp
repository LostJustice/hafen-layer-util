(in-package :salem-layer-util)

(deflayer unknown-layer ()
  (:defnone (buf io) ()
    (copy-raw-to-file buf (concatenate 'string io ".unknown")))
  (:defnone-binary (in-file io buffer) ()
    (push-file-to-buffer buffer (concatenate 'string in-file ".unknown"))))
