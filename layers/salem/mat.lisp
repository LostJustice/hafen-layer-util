(in-package :salem-layer-util)

(deflayer mat ()
  ;;Material Layer
  ;;Produces: #.data - ASCII format
  ;;Contains: uint16 - id
  ;;          loop: String - thing
  ;;                if thing="col": float - amb[r]
  ;;                                float - amb[g]
  ;;                                float - amb[b]
  ;;                                float - amb[a]
  ;;                                float - dif[r]
  ;;                                float - dif[g]
  ;;                                float - dif[b]
  ;;                                float - dif[a]
  ;;                                float - spc[r]
  ;;                                float - spc[g]
  ;;                                float - spc[b]
  ;;                                float - spc[a]
  ;;                                float - shine
  ;;                                float - emi[r]
  ;;                                float - emi[g]
  ;;                                float - emi[b]
  ;;                                float - emi[a]
  ;;                if thing="tex": uint16 - id
  ;;                if thing="texlink": String - name
  ;;                                    uint16 - ver
  ;;                                    uint16 - id
  ;;                if thing="light": String - light
  ;;                else: Invalid-Layer
  (:defdata (buf io off out) ()
    (write-line ";Material layer data file" out)
    ;;uint16 id
    (setf off (rw-uint buf off 2 out ";ID [id]:[unsigned-int-16]:"))
    (do ()
        ((>= off (length buf)))
      (multiple-value-bind (noff thing)
          (rw-str buf off out ";String [thing (col,linear,mipmap,nofacecull,tex,texlink,light,)]:")
        (case (intern thing)
          (|col| (setf noff (rw-floats buf noff out ";~A [~A]:[float]:"
                                       '(("Ambient Light[r]" "amb")
                                         ("Ambient Light[g]" "amb")
                                         ("Ambient Light[b]" "amb")
                                         ("Ambient Light[a]" "amb")
                                         ("Diffuse Light[r]" "dif")
                                         ("Diffuse Light[g]" "dif")
                                         ("Diffuse Light[b]" "dif")
                                         ("Diffuse Light[a]" "dif")
                                         ("Specular Light[r]" "spc")
                                         ("Specular Light[g]" "spc")
                                         ("Specular Light[b]" "spc")
                                         ("Specular Light[a]" "spc")
                                         ("Ambient Light[r]" "shine")
                                         ("Emi Light[r]" "emi")
                                         ("Emi Light[g]" "emi")
                                         ("Emi Light[b]" "emi")
                                         ("Emi Light[a]" "emi")))))
          (|linear| t)
          (|mipmap| t)
          (|nofacecull| t)
          (|tex| (setf noff (rw-uint buf noff 2 out ";ID [id]:[unsigned-int-16]:")))
          (|texlink| (setf noff (rw-str buf noff out ";String [name]:"))
                     (setf noff (rw-uints buf noff 2 out ";~A [~A]:[unsigned-int-16]"
                                          '(("Version" "ver") ("ID" "id")))))
          (|light| (setf noff (rw-str buf noff out ";String [light (pv,pp,vc,pc,n)]:")))
          (t (format t "###Error: make-fold-mat, invalid thing###~%")
             (return-from make-fold-mat)))
        (setf off noff))))
  
  ;;Material Layer
  ;;Contains: uint16
  ;;          Loop: String - thing
  ;;                if thing="col": floatx17
  ;;                   thing="tex": uint16
  ;;                   thing="texlink": String
  ;;                                    uint16x2
  ;;                   thing="light": String
  (:defdata-binary (in-file io buffer in) ()
    ;;uint16
    (rinte in 2 buffer)
    ;;Loop: String - thing
    (do ((thing (readin-next in)
                (readin-next in)))
        ((zerop (length thing)))
      (stre thing buffer)
      (case (intern thing)
        ;;floatx17
        (|col| (ntimes 17 (rfloate in buffer)))
        ;;uint16
        (|tex| (rinte in 2 buffer))
        ;;String
        (|texlink| (rstre in buffer)
                   ;;uint16x2
                   (ntimes 2 (rinte in 2 buffer)))
        ;;String
        (|light| (rstre in buffer))
        ;;Things that do nothing
        ((|linear| |mipmap| |nofacecull|) t)
        (t (format t "###ERROR: INVALID THING[~A] FOR MATERIAL LAYER###~%" thing))))))
