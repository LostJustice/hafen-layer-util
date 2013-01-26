(in-package :salem-layer-util)

(deflayer codeentry ()
  ;;CodeEntry Layer
  ;;Produces: #.data - ASCII format
  ;;Contains: loop: uint8 - t
  ;;                loop t=1: String - en [Notes: final en is "\0"]
  ;;                          String - cn [Notes: final cn is "\0"]
  ;;                loop t=2: String - ln [Notes: final ln is "\0"]
  ;;                          uint16 - ver
  (:defdata (buf io off out) ()
    (write-line ";CodeEntry Layer data file" out)
    (while (< off (length buf))
      ;;uint8 t
      (let ((type (aref buf off)))
        (incf off)
        (write-line ";Data Type [t]:[unsigned-byte (1-2)]:" out)
        (write-line (write-to-string type) out)
        ;;parse type
        (case type
          ;;loop t=1
          (1 (block t-1-loop
               (loop
                  ;;String en
                  (multiple-value-bind (noff en)
                      (rw-str buf off out ";String [EN]:")
                                        ;String cn
                    (setf noff (rw-str buf noff out ";String [CN]:"))
                    (when (zerop (length en))
                      (write-line 
                       ";Note: Final [EN/CN] lines must be blank as seen above" out)
                      (setf off noff)
                      (return-from t-1-loop))
                    (setf off noff)))))
          ;;loop t=2
          (2 (block t-2-loop
               (loop
                  ;;String ln
                  (multiple-value-bind (noff ln)
                      (rw-str buf off out ";String [LN]:")
                    (when (zerop (length ln))
                      (write-line
                       ";Note: Final [LN] line must be blank as seen above" out)
                      (setf off noff)
                      (return-from t-2-loop))
                    ;;uint16 ver
                    (setf off 
                          (rw-uint buf noff 2 out 
                                   ";Version [ver]:[unsigned-int-16]:"))))))))))
  ;;CodeEntry Layer
  ;;Contains: loop: uint8 - t
  ;;                loop t=1: String - en 
  ;;                          String
  ;;                          if en=='\0': break
  ;;                loop t=2: String - ln
  ;;                          if ln=='\0': break
  ;;                          uint16
  (:defdata-binary (in-file io buffer in) ()
    ;;Loop: uint8 - t
    (do ((type (readin-int in)
               (readin-int in)))
        ((eq type :eof))
      (inte type 1 buffer)
      (case type
        ;;Loop: String - en
        (1 (do ((en (readin-next in)
                    (readin-next in)))
               ((zerop (length en)))
             (stre en buffer)
             ;;String
             (rstre in buffer))
           ;;Read in String ['\0']
           (readin-next in)
           ;;Final en/String are '\0' [0x0000]
           (inte 0 2 buffer))
        ;;Loop: String - ln
        (2 (do ((ln (readin-next in)
                    (readin-next in)))
               ((zerop (length ln)))
             (stre ln buffer)
             ;;uint16
             (rinte in 2 buffer))
           ;;Final ln is '\0' [0x00]
           (inte 0 1 buffer))
        (t (format t "###ERROR: INVALID CODEENTRY LAYER###~%"))))))
