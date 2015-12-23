(in-package :hlu)


(defstruct layer
  name
  data-needed
  dfun
  efun)


(defun write-to-file (data fn)
  (with-open-file (fd fn
                      :direction :output
                      :if-exists :supersede
                      :element-type `(unsigned-byte 8))
    (write-sequence data fd)))  

(defmacro deflayer (name (data-p) dfun efun)
  `(register-layer
    (symbol-name (quote ,name))
    ,data-p
    ,efun
    ,dfun))

(let ((layers (make-hash-table :test 'equal)))
  (defun register-layer (layer-name
                         data-needed
                         encode-fun
                         decode-fun)
    (setf (gethash layer-name layers)
          (make-layer :name layer-name
                      :data-needed data-needed
                      :efun encode-fun
                      :dfun decode-fun)))


  (defun eparse-layer (lnm out base-fn)
    (let* ((layer (gethash (string-upcase lnm) layers))
           (buffer
            (flexi-streams:with-output-to-sequence (buf :element-type `(unsigned-byte 8))
              (if layer
                  (if (layer-data-needed layer)
                      (with-open-file (fd (strcat base-fn ".ini"))
                        (funcall (layer-efun layer) base-fn buf fd))
                      (funcall  (layer-efun layer) base-fn buf))
                  (encoder nil buf
                    :raw (strcat base-fn ".unknown"))))))
      (estring out lnm)
      (eint out (length buffer) 4)
      (write-sequence buffer out)))
  
  (defun dparse-layer (lnm msg base-fn)
    (ensure-directories-exist base-fn)
    (let ((layer (gethash (string-upcase lnm) layers)))
      (if layer
          (if (layer-data-needed layer)
              (with-open-file (fd (strcat base-fn ".ini")
                                  :direction :output
                                  :if-exists :supersede)
                (funcall (layer-dfun layer) msg base-fn fd))
              (funcall (layer-dfun layer) msg base-fn))
          ;;unknown layer
          (decoder msg nil
            :raw :all (strcat base-fn ".unknown"))))))
