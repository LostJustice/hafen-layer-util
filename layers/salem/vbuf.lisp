(in-package :salem-layer-util)

(deflayer vbuf ()
  ;;VertexBuffer Layer
  ;;Produces: #.data - ASCII format
  ;;Contains: uint8   - fl
  ;;          uint-16 - num
  ;;          loop->: uint8 - id
  ;;                  if id==0: loop(num*3): float - pos[i]
  ;;                  if id==1: loop(num*3): float - nrm[i]
  ;;                  if id==2: loop(num*2): float - tex[i]
  ;;                  if id==3: uint8 - mba
  ;;                            loop: String - bone
  ;;                                  if bone='\0': break
  ;;                                  loop: uint16 - run
  ;;                                        uint16 - st
  ;;                                        if run==0:break
  ;;                                        loop(run): float - w
  (:defdata (buf io off out) ()
    (write-line ";VertexBuffer layer data file" out)
    ;;uint8 fl
    (setf off (rw-uint buf off 1 out ";FL [fl]:[unsigned-byte]:"))
    ;;uint16 num
    (multiple-value-bind (noff num)
        (rw-uint buf off 2 out ";Num [num]:[unsigned-int-16]:")
      (while (< noff (length buf))
        ;;uint8 id
        (multiple-value-bind (uboff ub)
            (rw-uint buf noff 1 out ";ID [id]:[unsigned-byte (0-3)]:")
          (case ub
            (0 (format out ";Note: # of POS floats = Num*3~%") 
               (dotimes (i (3* num))
                 ;;floats pos[i]
                 (setf uboff (rw-float buf uboff out (format nil 
                                                             ";Pos#~A [pos]:[float]"
                                                             i)))))
            (1 (format out ";Note: # of NRM floats = Num*3~%")
               (dotimes (i (3* num))
                 ;;floats nrm[i]
                 (setf uboff (rw-float buf uboff out (format nil 
                                                             ";Nrm#~A [nrm]:[float]"
                                                             i)))))
            (2 (format out ";Note: # of TEX floats = Num*2~%")
               (dotimes (i (2* num))
                 ;;floats tex[i]
                 (setf uboff (rw-float buf uboff out (format nil 
                                                             ";Tex#~A [tex]:[float]"
                                                             i)))))
            ;;uint8 mba
            (3 (setf uboff (rw-uint buf uboff 1 out ";MBA [mba]:[unsigned-byte]"))
               (block bone-loop
                 (loop
                    ;;bone == '\0'
                    (if (zerop (aref buf uboff))
                        (progn
                          ;;Final bone is '\0'
                          (write-line 
                           ";NOTE: final [bone] line must be blank as followed" 
                           out)
                          (write-line ";String [bone]:" out)
                          (write-line "" out)
                          (incf uboff)
                          (return-from bone-loop))
                        (progn
                          ;;String bone
                          (setf uboff (rw-str buf uboff out ";String [bone]:"))
                          (block run-loop
                            (loop 
                               ;;uint16 run
                               (write-line ";Note: final RUN must be 0" out)
                               (multiple-value-bind (runoff run)
                                   (rw-uint buf uboff 2 out 
                                            ";RUN [run]:[unsigned-int-16]")
                                 ;;uint16 st 
                                 (setf runoff (rw-uint buf runoff 2 out
                                                       ";ST [st]:[unsigned-int-16]"))
                                 (when (zerop run)
                                   ;;break if run == 0
                                   (setf uboff runoff)
                                   (return-from run-loop))
                                 (dotimes (i run)
                                   (setf runoff (rw-float buf runoff out
                                                          (format nil 
                                                                  ";W#~A [w]:[float]:"
                                                                  i))))
                                 (setf uboff runoff))))))))))
          (setf noff uboff)))))  
  ;;VertexBuffer Layer
  ;;Contains: uint8
  ;;          uint16 - num
  ;;          Loop: uint8 - id
  ;;                if id==0: loop(num*3): float
  ;;                   id==1: loop(num*3): float
  ;;                   id==2: loop(num*2): float
  ;;                   id==3: uint8
  ;;                          Loop: String - bone
  ;;                                if bone=='\0': break
  ;;                                Loop: uint16 - run
  ;;                                      uint16
  ;;                                      if run==0: break
  ;;                                      loop(run): float
  (:defdata-binary (in-file io buffer in) ()
    ;;uint8
    (rinte in 1 buffer)
    ;;uint16 - num
    (let ((num (readin-int in)))
      (inte num 2 buffer)
      ;;Loop: uint8 - id
      (do ((id (readin-int in)
               (readin-int in)))
          ((eq id :eof))
        (inte id 1 buffer)
        (case id
          ;;Loop(num*3): float
          (0 (ntimes (* num 3) (rfloate in buffer)))
          ;;Loop(num*3): float
          (1 (ntimes (* num 3) (rfloate in buffer)))
          ;;Loop(num*2): float
          (2 (ntimes (* num 2) (rfloate in buffer)))
          ;;uint8
          (3 (rinte in 1 buffer)
             ;;Loop: String - bone
             (do ((bone (readin-next in)
                        (readin-next in)))
                 ((zerop (length bone)))
               (stre bone buffer)
               ;;Loop: uint16 - run
               (do ((run (readin-int in)
                         (readin-int in)))
                   ((zerop run))
                 (inte run 2 buffer)
                 ;;uint16
                 (rinte in 2 buffer)
                 ;;Loop(run)
                 (ntimes run
                   ;;float
                   (rfloate in buffer)))
               ;;Final run/uint16 are both 0 [0x0000]
               (readin-int in) ;wasted ST
               (inte 0 4 buffer))
             ;;Final bone is '\0' [0x00]
             (inte 0 1 buffer))
          (t (format t "###ERROR: INVALID ID[~A] FOR VERTEX BUFFER LAYER###~%" id)))))))
