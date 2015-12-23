(in-package :hlu)

(deflayer code ()
  ;;cstring - name
  ;;.class - java bytecode
  (:defdata (buf io off out) ()
    :cstring "name"
    (copy-raw-to-file buf (concatenate 'string io ".class") :start off))
  (:defdata-binary (in-file io buffer in) ()
    :cstring
    (push-file-to-buffer buffer (concatenate 'string in-file ".class"))))
