(in-package :salem-layer-util)

(deflayer code ()
  ;;string - name
  ;;.class - java bytecode
  (:defdata (buf io off out) ()
    (write-line ";Code Layer data file" out)
    :string "name"
    (copy-raw-to-file buf (concatenate 'string io ".class") :start off))
  (:defdata-binary (in-file io buffer in) ()
    :string
    (push-file-to-buffer buffer (concatenate 'string in-file ".class"))))
