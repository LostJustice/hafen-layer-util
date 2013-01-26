(in-package :salem-layer-util)

(deflayer mesh ()
  ;;Mesh Layer
  ;;Produces: #.data - ASCII format
  ;;Contains: uint8  - fl
  ;;          uint16 - num
  ;;          uint16 - matid
  ;;          if (fl&2)!=0: uint16 - id
  ;;          if (fl&4)!=0: uint16 - ref
  ;;          if (fl&~7)!=0: Invalid Layer
  ;;          loop(num*3): uint16 - ind[i]
  (:defdata (buf io off out) ()
    (write-line ";Mesh layer data file" out)
    ;;uint8 fl
    (multiple-value-bind (floff fl)
        (rw-uint buf off 1 out ";FL [fl]:[unsigned-byte]:")
      ;;uint16 num
      (multiple-value-bind (noff num)
          (rw-uint buf floff 2 out ";Ind Length [num]:[unsigned-int-16]:")
        ;;uint16 matid
        (setf noff (rw-uint buf noff 2 out ";Material ID [matid]:[unsigned-int-16]~%"))
        (format out ";Note: ID will only be needed to follow right after this if:~%")
        (format out ";      (FL & 2)!=0~%")
        (format out ";Note: REF will only be needed to follow right after this and~%")
        (format out ";      right after ID if there, if: (FL & 4)!=0~%")
        (format out ";Note: (FL & ~~7) should always equal 0!!!~%")
        (when (/= (logand fl 2) 0)
          ;;uint16 id
          (setf noff (rw-uint buf noff 2 out ";ID [id]:[unsigned-int-16]:")))
        (when (/= (logand fl 4) 0)
          ;;uint16 ref
          (setf noff (rw-uint buf noff 2 out ";REF [ref]:[unsigned-int-16]:")))
        (when (/= (logand fl (lognot 7)) 0)
          (format t "###ERROR: make-fold-mesh invalid fl###~%")
          (return-from make-fold-mesh))
        (format out ";Note: Ind Length * 3 = # of Inds~%")
        (dotimes (i (* num 3))
          ;;uint16 ind[i]
          (setf noff (rw-uint buf noff 2 out (format nil 
                                                     ";Ind#~A [ind]:[unsigned-int-16]:"
                                                     i)))))))  
  ;;Mesh Layer
  ;;Contains: uint8 - fl
  ;;          uint16 - num
  ;;          uint16
  ;;          if (fl&2)!=0: uint16
  ;;          if (fl&4)!=0: uint16
  ;;          if (fl&~7)!=0: Invalid Layer
  ;;          loop(num*3): uint16
  (:defdata-binary (in-file io buffer in) ()
    ;;uint16 fl,uint16 nm
    (let ((fl (readin-int in))
          (num (readin-int in)))
      (inte fl 1 buffer)
      (inte num 2 buffer)
      ;;uint16
      (rinte in 2 buffer)
      ;;if (fl&2)!=0:
      (when (/= (logand fl 2) 0)
        ;;uint16
        (rinte in 2 buffer))
      ;;if (fl&4)!=0:
      (when (/= (logand fl 4) 0)
        ;;uint16
        (rinte in 2 buffer))
      ;;if (fl&~7)!=0: Invalid layer
      (when (/= (logand fl (lognot 7)) 0)
        (format t "###ERROR: INVALID MESH LAYER (FL&~~7!=0)###~%"))
      ;;Loop(num*3)
      (ntimes (* num 3)
        ;;uint16
        (rinte in 2 buffer)))))
