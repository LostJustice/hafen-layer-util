(in-package :salem-layer-util)

(deflayer code ()
  ;;Code layer
  ;;cstring - name
  ;;BUFFER - .class
  (:defdata (buf io off out) ()
    :cstring "class name"
    (copy-raw-to-file buf (concatenate 'string io ".class") :start off))
  (:defdata-binary (in-file io buffer in) ()
    :cstring
    (push-file-to-buffer buffer (concatenate 'string in-file ".class"))))
