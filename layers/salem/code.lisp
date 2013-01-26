(in-package :salem-layer-util)

(deflayer code ()
  ;;Code Layer
  ;;Produces: #.data - ASCII format
  ;;          #.class - java class file
  ;;Contains: String - name
  ;;          BUFFER - code : java.class
  (:defdata (buf io off out) ()
    (write-line ";Code Layer data file" out)
    ;;String name
    (setf off (rw-str buf off out ";String [name]:"))
    ;;Write code for java class
    (copy-raw-to-file buf (concatenate 'string io ".class") :start off))

  ;;Code Layer
  ;;Contains: String
  ;;          #.class, binary file
  (:defdata-binary (in-file io buffer in) ()
    ;;String
    (rstre in buffer)
    ;;#.class
    (push-file-to-buffer (buffer (concatenate 'stirng in-file ".class")))))
