(in-package :salem-layer-util)

;; simple macro for maintaining repeativeness from each layer
(defmacro deflayer (name type &body body)
  "Simplifies layer code just a tiny bit
ARGS:
type => Specifies the type of layer 
        :none, :data
INNERARGS for :none and :data,
buf => data buffer, an array of unsigned-bytes
io  => output file, the output base pathname

INNERLOCALS for :none and :data,
off => offset within buffer/io
if type :data
OUT => out iostream for data file

INNERARGS for :binary,
in-file => input file, the input base pathname
io      => A Output IO-STREAM, unsigned-byte for .res file binary 

INNERLOCALS for :binary
buffer  => An array that shall old all the written bytes before actually being
           written to IO
"
  (case type
    (:undef `(defun ,name () (format t "Undefined Function ~A~%" ',name)))
    (:none
     `(defun ,name (buf io)
        (when *verbose*
          (format t "   Layer: ~A~%" ',name)
          (format t "   Len  : ~A~%" (length buf)))
        ,@body))
    (:data
     `(defun ,name (buf io)
        (when *verbose*
          (format t "   Layer: ~A~%" ',name)
          (format t "   Len  : ~A~%" (length buf)))
        (let ((off 0))
          (with-open-file (out (concatenate 'string
                                            io
                                            ".data")
                               :direction :output
                               :if-exists :supersede)
            ,@body))))
    (:data-binary
     `(defun ,name (in-file io)
        (when *verbose*
          (format t "   Layer: ~A~%" ',name))
        (let ((buffer (make-array 1 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0)))
          (with-open-file (in (concatenate 'string
                                           in-file
                                           ".data")
                              :direction :input)
            ,@body)
          ;;write data
          (when *verbose*
            (format t "   Len  : ~A~%" (length buffer)))
          (write-sequence (int->ubarr (length buffer) 4) io)
          (write-sequence buffer io))))
    (:none-binary
     `(defun ,name (in-file io)
        (when *verbose*
          (format t "   Layer: ~A~%" ',name))
        (let ((buffer (make-array 1 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0)))
          ,@body
          ;;write data
          (when *verbose*
            (format t "   Len  : ~A~%" (length buffer)))
          (write-sequence (int->ubarr (length buffer) 4) io)
          (write-sequence buffer io))))
    (t
     `(defun ,name ()
        (format t "   ERROR MAKING LAYER ~A~%" ',name)))))


;;;Res/Cache file -> Folder format
;;; BUF ==> Array Buffer
;;; IO  ==> filepath [String]

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
(deflayer make-fold-vbuf :data
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

;;Mesh Layer
;;Produces: #.data - ASCII format
;;Contains: uint8  - fl
;;          uint16 - num
;;          uint16 - matid
;;          if (fl&2)!=0: uint16 - id
;;          if (fl&4)!=0: uint16 - ref
;;          if (fl&~7)!=0: Invalid Layer
;;          loop(num*3): uint16 - ind[i]
(deflayer make-fold-mesh :data
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
(deflayer make-fold-mat :data
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

;;Skeleton Layer
;;Produces: #.data - ASCII format
;;Contains: loop: String - bnm
;;                float  - pos[x]
;;                float  - pos[y]
;;                float  - pos[z]
;;                float  - rax[x]
;;                float  - rax[y]
;;                float  - rax[z]
;;                float  - rang
;;                String - bp
(deflayer make-fold-skel :data
  (write-line ";Skeleton layer data file" out)
  (do ()
      ((>= off (length buf)))
    ;;String bnm
    (setf off (rw-str buf off out ";String [bname]:"))
    ;;floats pos[x,y,z],rax[x,y,z]
    (setf off (rw-floats buf off out ";~A [~A]:[float]:"
                         '(("Pos[x]" "pos") ("Pos[y]" "pos")
                           ("Pos[z]" "pos") ("Rax[x]" "rax")
                           ("Rax[y]" "rax") ("Rax[z]" "rax")
                           ("RANG" "rang"))))
    ;;String bp
    (setf off (rw-str buf off out ";String [bp]:"))))

;;ResPose Layer
;;Produces: #.data - ASCII format
;;Contains: uint16 - id
;;          uint8  - fl
;;          uint8  - mode
;;          float  - len
;;          if (fl&1)!=0: float  - nspeed
;;          loop: String - bnm
;;                uint16 - Frames Length
;;                loop(^): float - tm
;;                         loop(3): float - trans[o]
;;                         float - rang
;;                         loop(3): float - rax[o]
(deflayer make-fold-skan :data
  (write-line ";ResPose layer data file" out)
  ;;uint16 id
  (setf off (rw-uint buf off 2 out ";ID [id]:[unsigned-int-16]:"))
  ;;uint8 fl
  (multiple-value-bind (floff fl)
      (rw-uint buf off 1 out ";FL [fn]:[unsigned-bute]")
    ;;uint8 mode
    (format out ";Note: Modes include:0 => ONCE, 1 => LOOP, 2 => PONG, 3 => PONGLOOP")
    (setf floff (rw-uint buf floff 1 out ";Mode [mode]:[unsigned-byte (0-3)]:"))
    ;;float len
    (setf floff (rw-float buf floff out ";Length  [len]:[float]:"))
    ;;float nspeed
    (when (/= (logand fl 1) 0)
      (setf floff (rw-float buf floff out ";NSpeed [nspeed]:[float]:")))
    (do ()
        ((>= floff (length buf)))
      ;;String bnm
      (setf floff (rw-str buf floff out ";String [bname]:"))
      ;;uint16 Frames Length
      (multiple-value-bind (noff flen)
          (rw-uint buf floff 2 out ";Frames Length []:[unsigned-int-16]:")
        (dotimes (i flen)
          ;;floats tm,trans[0-2],rang,rax[0-2]
          (setf noff (rw-floats buf noff out ";~A#~A [~A]:[float]:"
                                `(("TM" ,i "tm")          ("Trans[0]" ,i "trans")
                                  ("Trans[1]" ,i "trans") ("Trans[2]" ,i "trans")
                                  ("RANG" ,i "rang")      ("Rax[0]" ,i "rax")
                                  ("Rax[1]" ,i "rax")     ("Rax[2]" ,i "rax")))))
        (setf floff noff)))))

;;BoneOffset Layer
;;Produces: #.data - ASCII format
;;Contains: String - name
;;          loop: uint8 - opcode
;;                if opcode=0: float - x
;;                             float - y
;;                             float - z
;;                if opcode=1: float - angle
;;                             float - ax
;;                             float - ay
;;                             float - az
;;                if opcode=2: String- Bone Name
;;                if opcode=3: float - rx1
;;                             float - ry1
;;                             float - rz1
;;                             String- Original Name
;;                             String- Target Name
(deflayer make-fold-boneoff :data
  (write-line ";Bone Offset layer data file" out)
  ;;String name
  (setf off (rw-str buf off out ";String [name]:"))
  (do ()
      ((>= off (length buf)))
    ;;uint8 opcode
    (multiple-value-bind (noff opcode)
        (rw-uint buf off 1 out ";Opcode []:[unsigned-byte (0-3)]:")
      (case opcode
        ;;floats x,y,z
        (0 (setf noff (rw-floats buf noff out ";~A [~A]:[float]:"
                                 '(("X" "x") ("Y" "y") ("Z" "z")))))
        ;;floats angle,ax,ay,az
        (1 (setf noff (rw-floats buf noff out ";~A [~A]:[float]:"
                                 '(("Angle" "ang") ("AX" "ax")
                                   ("AY" "ay") ("AZ" "az")))))
        ;;String bonenm
        (2 (setf noff (rw-str buf noff out ";String [Bone Name]:")))
        ;;floats rx1,ry1,rz1
        (3 (setf noff (rw-floats buf noff out ";~A [~A]:[float]:"
                                 '(("RX1" "rx1") ("RY1" "ry1") ("RZ1" "rz1"))))
           ;;Strings orignm, tgtnm
           (setf noff (rw-strs buf noff out ";String [~A]:"
                               '("Original Name" "Target Name")))))
      (setf off noff))))

;;Light Layer
;;Produces: #.data - ASCII format
;;Contains: sint16  - id
;;          floatx4 - amb
;;          floatx4 - dif
;;          floatx4 - spc
;;          uint8   - t
;;          if t=1: float - ac
;;                  float - al
;;                  float - aq
;;          if t=2: float - x
;;                  float - y
;;                  float - z
;;          if t=3: float - exp
(deflayer make-fold-light :data
  (write-line ";Light layer data file" out)
  ;;sint16 id
  (setf off (rw-sint buf off 2 out ";ID [id]:[signed-int-16]:"))
  ;;floatx4 amb
  (setf off (rw-floats buf off out ";Ambient Light [~A] [amb]:[float32]:" 
                       '("r" "g" "b" "a")))
  ;;floatx4 dif
  (setf off (rw-floats buf off out ";Diffuse Light [~A] [dif]:[float32]:"
                       '("r" "g" "b" "a")))
  ;;floatx4 spc
  (setf off (rw-floats buf off out ";Specular Light [~A] [spec]:[float32]:"
                       '("r" "g" "b" "a")))
  ;;uint8 type
  (multiple-value-bind (noff type)
      (rw-uint buf off 1 out ";Type [t]:[unsigned-byte]:")
    (case type
      ;;float ac,al,aq
      (1 (setf noff (rw-floats buf noff out ";~A [~A]:[float32]:"
                              '(("AC" "ac") ("AL" "al") ("AQ" "aq")))))
      ;;float x,y,z
      (2 (setf noff (rw-floats buf noff out ";~A [~A]:[float32]:"
                              '(("X" "x") ("Y" "y") ("Z" "z")))))
      ;;float exp
      (3 (setf noff (rw-float buf noff out ";EXP [exp]:[float32]:")))
      ;;invalid layer
      (t (format t "###Error: Light Layer type not 1-3###~%")))))

;;RLink Layer
;;Produces: #.data - ASCII format
;;Contains: uint8  - t
;;          String - meshnm
;;          uint16 - meshver
;;          uint16 - meshid
;;          String - matnm
;;          uint16 - matver
;;          uint16 - matid
(deflayer make-fold-rlink :data
  (write-line ";Rlink layer data file" out)
  ;;uint8 t
  (setf off (rw-uint buf off 1 out ";T [t]:[unsigned-byte]:"))
  ;;String meshnm
  (setf off (rw-str buf off out ";String Mesh Name [meshnm]:"))
  ;;uint16 meshver,meshid
  (setf off (rw-uints buf off 2 out ";~A [~A]:[unsigned-int-16]:"
                      '(("Mesh Version" "meshver")
                        ("Mesh ID" "meshid"))))
  ;;String matnm
  (setf off (rw-str buf off out ";String Mat Name [matnm]:"))
  ;;uint16 matver,matid
  (setf off (rw-uints buf off 2 out ";~A [~A]:[unsigned-int-16]:"
                      '(("Mat Version" "matver")
                        ("Mat ID" "matid")))))
  
;;Image Layer
;;Produces: #.data - ASCII format
;;          #.png  - png image
;;Contains: uint16 - z
;;          uint16 - subz
;;          uint8  - nooff OBSOLETE
;;          uint16 - id
;;          uint16 - o[x]
;;          uint16 - o[y]
;;          BUFFER - img : png
(deflayer make-fold-image :data
  (write-line ";Image Layer data file" out)
  ;;uint16 z,subz
  (setf off (rw-uints buf off 2 out ";~A [~A]:[unsigned-int-16]:"
                      '(("Z" "z") ("Subz" "subz"))))
  ;;uint8 nooff
  (setf off (rw-uint buf off 1 out ";nooff [noffo]:[unsigned-byte][OBSOLETE]:"))
  ;;uint16 id,o[x],o[y]
  (setf off (rw-uints buf off 2 out ";~A [~A]:[unsigned-int-16]:"
                      '(("ID" "id") ("O[x]" "o") ("O[y]" "o"))))
  ;;BUFFER
  (with-open-file (outi (concatenate 'string
                                     io
                                     ".png")
                        :direction :output
                        :if-exists :supersede
                        :element-type 'unsigned-byte)
    (write-sequence buf outi :start off)))
    ;;(write-till buf outi off (- (length buf) off 1))))

;;Tooltip Layer
;;Produces: #.data - ASCII format
;;Contains: String - text
(deflayer make-fold-tooltip :data
  (incf off)
  (write-line ";Tooltip layer data file" out)
  ;;string text
  (write-line ";String [text]:" out)
  (write-line (babel:octets-to-string buf :encoding :utf-8) out))

;;Tile Layer
;;Produces: #.data - ASCII format
;;          #.png  - png image
;;Contains: uint8  - t
;;          uint8  - id
;;          uint16 - w
;;          BUFFER - img : png
(deflayer make-fold-tile :data
  (write-line ";Tile layer data file" out)
  ;;uint8 t,id
  (setf off (rw-uints buf off 1 out ";~A [~A]:[unsigned-byte]:"
                      '(("T" "t") ("ID" "id"))))
  ;;uint16 w
  (setf off (rw-uint buf off 2 out ";W [w]:[unsigned-int-16]:"))
  ;;BUFFER
  (with-open-file (outi (concatenate 'string
                                     io
                                     ".png")
                        :direction :output
                        :if-exists :supersede
                        :element-type 'unsigned-byte)
    (write-sequence buf outi :start off)))
;;(write-till buf outi off (- (length buf) off 1))))

;;Neg Layer
;;Produces: #.data - ASCII format
;;Contains: uint16 - cc[x] [off=0]
;;          uint16 - cc[y] [off=2]
;;          uint8  - en    [off=16]
;;          loop(en): uint8 - epid
;;                    uint16 - cn (ep[epid] Length)
;;                    loop(cn): uint16 - ep[epid][o].[x]
;;                              uint16 - ep[epid][o].[y]
(deflayer make-fold-neg :data
  (write-line ";Neg layer data file" out)
  ;;uint16 cc[x],cc[y]
  (setf off (rw-uints buf off 2 out ";~A [~A]:[unsigned-int-16]:"
                      '(("CC[x]" "cc") ("CC[y]" "cc"))))
  ;;uint8 en
  (multiple-value-bind (boff en)
      (rw-uint buf 16 1 out ";EN [en]:[unsigned-byte]:")
    (1+ boff) ;TODO: get en without boff
    (setf off 17)
    (do ((i 0 (1+ i)))
        ((>= i en))
      ;;uint8 epid
      (setf off (rw-uint buf off 1 out ";EPID [epid]:[unsigned-byte]:"))
      ;;uint16 cn
      (multiple-value-bind (noff cn)
          (rw-uint buf off 2 out ";CN Length [cn]:[unsigned-int-16]:")
        (do ((j 0 (1+ j)))
            ((>= j cn))
          ;;uint16 ep[epid][o].[x],ep[epid][o],[y]
          (setf noff (rw-uints buf noff 2 out ";ep[epid][~A] [~A]:[unsigned-int-16]"
                               '((j "x") (j "y")))))
        (setf off noff)))))

;;Anim Layer
;;Produces: #.data - ASCII format
;;Contains: uint16 - id
;;          uint16 - d
;;          uint16 - ids length
;;          loop(ids): uint16 - ids[i]
(deflayer make-fold-anim :data
  (write-line ";Anime layer data file" out)
  ;;uint16 id,d
  (setf off (rw-uints buf off 2 out ";~A [~A]:[unsigned-int-16]:"
                      '(("ID" "id") ("D" "d"))))
  ;;uint16 ids
  (multiple-value-bind (noff ids)
      (rw-uint buf off 2 out ";IDS Length [ids]:[unsigned-int-16]:")
    (do ((i 0 (1+ i)))
        ((>= i ids))
      ;;uint16 ids[i]
      (setf noff (rw-uint buf noff 2 out (format nil 
                                                ";IDS#~A [ids]:[unsigned-int-16]:"
                                                i))))))
;;Tileset Layer
;;Produces: #.data - ASCII format
;;Contains: uint8  - fl
;;          uint16 - flnum [# of fln/flv/flws]
;;          uint16 - flavprob
;;          Loop(flnum): String - fln
;;                       uint16 - flv
;;                       uint8  - flw
(deflayer make-fold-tileset :data
  (write-line ";Tileset layer data file" out)
  ;;uint8 fl
  (setf off (rw-uint buf off 1 out ";FL [fl]:[unsigned-byte]:"))
  ;;uint16 flnum
  (multiple-value-bind (noff flnum)
      (rw-uint buf off 2 out ";FLNum [flnum]:[unsigned-int-16]:")
    ;;uint16 flavprob
    (setf noff (rw-uint buf noff 2 out ";flavprob [flavprob]:[unsigned-int-16]:"))
    (do ((i 0 (1+ i)))
        ((>= i flnum))
      ;;String fln
      (setf noff (rw-str buf off out (format nil ";String#~A [fln]" i)))
      ;;uint16 flv
      (setf noff (rw-uint buf off 2 out (format nil 
                                                ";flv#~A [flv]:[unsigned-int-16]:"
                                                i)))
      ;;uint8 flw
      (setf noff (rw-uint buf off 1 out (format nil 
                                                ";flw#~A [flw]:[unsigned-int-16]:"
                                                i))))))
  
;;Pagina Layer
;;Produces: #.data - ASCII format
;;Contains: String - text
(deflayer make-fold-pagina :data
  (incf off) ;;todo: remove
  (write-line ";Pagina layer data file" out)
  ;;string text
  (write-line ";String [text]:" out)
  (write-line (babel:octets-to-string buf :encoding :utf-8) out))

;;Action Layer
;;Produces: #.data - ASCII format
;;Contains: String - pr
;;          uint16 - pver
;;          String - name
;;          String - preq
;;          uint16 - hk
;;          uint16 - ad [Array length]
;;          Loop: String - ad
(deflayer make-fold-action :data
  (write-line ";Action layer data file" out)
  ;;String pr
  (setf off (rw-str buf off out ";String [pr]:"))
  ;;uint16 pver
  (setf off (rw-uint buf off 2 out ";PVersion [pver]:[unsigned-int-16]:"))
  ;;String name,preq
  (setf off (rw-strs buf off out ";String [~A]:"
                     '("name" "preq:OBSOLETE")))
  ;;uint16 hk
  (setf off (rw-uint buf off 2 out ";HK [hk]:[unsigned-int-16]:"))
  ;;uint16 ad
  (multiple-value-bind (noff ad)
      (rw-uint buf off 2 out ";AD Length [ad]:[unsigned-int-16]:")
    (dotimes (i ad)
      ;;String ad
      (setf noff (rw-str buf noff out (format nil ";String#~A [ad]:" i))))))

;;Code Layer
;;Produces: #.data - ASCII format
;;          #.class - java class file
;;Contains: String - name
;;          BUFFER - code : java.class
(deflayer make-fold-code :data
  (write-line ";Code Layer data file" out)
  ;;String name
  (setf off (rw-str buf off out ";String [name]:"))
  ;;Write code for java class
  (with-open-file (outc (concatenate 'string
                                     io
                                     ".class")
                        :direction :output
                        :if-exists :supersede
                        :element-type 'unsigned-byte)
    (write-sequence buf outc :start off)))
    ;;write-rest of data
    ;(write-till buf outc off (- (1- (length buf)) off))))

;;CodeEntry Layer
;;Produces: #.data - ASCII format
;;Contains: loop: uint8 - t
;;                loop t=1: String - en [Notes: final en is "\0"]
;;                          String - cn [Notes: final cn is "\0"]
;;                loop t=2: String - ln [Notes: final ln is "\0"]
;;                          uint16 - ver
(deflayer make-fold-codeentry :data
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

;;Audio layer
;;Produces: #.ogg - binary format
;;Contains: BUFFER - raw : ogg
(deflayer make-fold-audio :none
  (with-open-file (out (concatenate 'string
                                    io
                                    ".ogg")
                       :direction :output
                       :if-exists :supersede
                       :element-type 'unsigned-byte)
    (write-sequence buf out)))
    ;;(write-till buf out 0 (1- (length buf)))))

;;Midi layer
;;Produces: #.midi - binary format
;;Contains: BUFFER - raw : midi format
(deflayer make-fold-midi :none
  (with-open-file (out (concatenate 'string
                                    io
                                    ".midi")
                       :direction :output
                       :if-exists :supersede
                       :element-type 'unsigned-byte)
   (write-sequence buf out)))
    ;;(write-till buf out 0 (1- (length buf)))))



;;;Folder -> Res/Cache format
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
(deflayer make-file-vbuf :data-binary
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
        (t (format t "###ERROR: INVALID ID[~A] FOR VERTEX BUFFER LAYER###~%" id))))))

;;Mesh Layer
;;Contains: uint8 - fl
;;          uint16 - num
;;          uint16
;;          if (fl&2)!=0: uint16
;;          if (fl&4)!=0: uint16
;;          if (fl&~7)!=0: Invalid Layer
;;          loop(num*3): uint16
(deflayer make-file-mesh :data-binary
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
      (rinte in 2 buffer))))

;;Material Layer
;;Contains: uint16
;;          Loop: String - thing
;;                if thing="col": floatx17
;;                   thing="tex": uint16
;;                   thing="texlink": String
;;                                    uint16x2
;;                   thing="light": String
(deflayer make-file-mat :data-binary
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
      (t (format t "###ERROR: INVALID THING[~A] FOR MATERIAL LAYER###~%" thing)))))

;;Skeleton Layer
;;Contains: Loop: String
;;                floatx7
;;                String
(deflayer make-file-skel :data-binary
  ;;Loop: String
  (do ((name (readin-next in)
             (readin-next in)))
      ((zerop (length name)))
    (stre name buffer)
    ;;floatx7
    (ntimes 7 (rfloate in buffer))
    ;;String
    (rstre in buffer)))

;;ResPose Layer
;;Contains: uint16
;;          uint8 - fl
;;          uint8
;;          float
;;          if (fl&1)!=0: float
;;          Loop: String
;;                uint16 -fls
;;                Loop(fls): float
;;                           floatx3
;;                           float
;;                           floatx3
(deflayer make-file-skan :data-binary
  ;;uint16
  (rinte in 2 buffer)
  ;;uint8 - fl
  (let ((fl (readin-int in)))
    (inte fl 1 buffer)
    ;;uint 8
    (rinte in 1 buffer)
    ;;float
    (rfloate in buffer)
    ;;if (fl&1)!=0:
    (when (/= (logand fl 1) 0)
      ;;float
      (rfloate in buffer))
    ;;Loop: String
    (do ((bone (readin-next in)
               (readin-next in)))
        ((zerop (length bone)))
      (stre bone buffer)
      ;;uint16 - fls
      (let ((fls (readin-int in)))
        (inte fls 2 buffer)
        ;;Loop(fls)
        (ntimes fls
          ;;floatx8
          (ntimes 8 (rfloate in buffer)))))))

;;BoneOffset Layer
;;Contains: String
;;          Loop: uint8 - opcode
;;                if opcode=0: floatx3
;;                   opcode=1: floatx4
;;                   opcode=2: String
;;                   opcode=3: floatx3
;;                             Stringx2
(deflayer make-file-boneoff :data-binary
  ;;String
  (rstre in buffer)
  ;;Loop: uint8 - opcode
  (do ((opcode (readin-int in)
               (readin-int in)))
      ((eq opcode :eof))
    (inte opcode 1 buffer)
    (case opcode
      ;;floatx3
      (0 (ntimes 3 (rfloate in buffer)))
      ;;floatx4
      (1 (ntimes 4 (rfloate in buffer)))
      ;;String
      (2 (rstre in buffer))
      ;;floatx3
      (3 (ntimes 3 (rfloate in buffer))
         ;;Stringx2
         (ntimes 2 (rstre in buffer)))
      (t (format t "###ERROR:INVALID OPCODE FOR BONE OFFSET LAYER###~%")))))

;;Light Layer
;;Contains: sint16
;;          floatx4
;;          floatx4
;;          floatx4
;;          uint8 - t
;;          if t=1: floatx3
;;             t=2: floatx3
;;             t=3: float
(deflayer make-file-light :data-binary
  ;;sint16
  (rinte in 2 buffer)
  ;;floatx12h
  (ntimes 12 (rfloate in buffer))
  ;;uint8 - t
  (let ((ty (readin-int in)))
    (inte ty 1 buffer)
    (case ty
      ;;floatx3
      (1 (ntimes 3 (rfloate in buffer)))
      ;;floatx3
      (2 (ntimes 3 (rfloate in buffer)))
      ;;float
      (3 (rfloate in buffer))
      (t (format t "###ERROR: INVALIDE TYPE FOR LIGHT LAYER###~%")))))
      
;;RLink Layer
;;Contains: uint8
;;          String
;;          uint16
;;          uint16
;;          String
;;          uint16
;;          uint16
(deflayer make-file-rlink :data-binary
  ;;uint8
  (rinte in 1 buffer)
  ;;x2
  (ntimes 2
    ;;String
    (rstre in buffer)
    ;;uint16x2
    (ntimes 2 (rinte in 2 buffer))))

;;Image Layer
;;Contains: uint16
;;          uint16
;;          uint8
;;          uint16
;;          uint16
;;          uint16
;;          #.png - binary file
(deflayer make-file-image :data-binary
  ;;uint16x2
  (ntimes 2 (rinte in 2 buffer))
  ;;uint8
  (rinte in 1 buffer)
  ;;uint16x3
  (ntimes 3 (rinte in 2 buffer))
  ;;#.png
  (with-open-file (in (concatenate 'string
                                   in-file
                                   ".png")
                      :direction :input
                      :element-type 'unsigned-byte)
    (let ((seq (make-array (file-length in)
                           :element-type '(unsigned-byte 8))))
      (read-sequence seq in)
      (doarr (byte seq)
        (vector-push-extend byte buffer)))))

;;Tooltip Layer
;;Contains: String
(deflayer make-file-tooltip :data-binary
  ;;String
  (rstre in buffer))

;;Tile Layer
;;Contains: uint8
;;          uint8
;;          uint16
;;          #.png - binary file
(deflayer make-file-tile :data-binary
  ;;uint8
  (rinte in 1 buffer)
  ;;uint8
  (rinte in 1 buffer)
  ;;uint16
  (rinte in 2 buffer)
  ;;#.png
  (with-open-file (in (concatenate 'string
                                   in-file
                                   ".png")
                      :direction :input
                      :element-type 'unsigned-byte)
    (let ((seq (make-array (file-length in)
                           :element-type '(unsigned-byte 8))))
      (read-sequence seq in)
      (doarr (byte seq)
        (vector-push-extend byte buffer)))))

;;Neg Layer
;;Contains: uint16 - OFF[0]
;;          uint16 - OFF[2]
;;          ignored - OFF[4-15]
;;          uint8  - en OFF[16]
;;          Loop(en): uint8
;;                    uint16 - cn
;;                    Loop(cn): uint16
;;                              uint16
(deflayer make-file-neg :data-binary
  ;;uint16
  (rinte in 2 buffer)
  ;;uint16
  (rinte in 2 buffer) 
  ;;ignore
  (inte 0 12 buffer)
  ;;uint8 - en
  (let ((en (readin-int in)))
    (inte en 1 buffer)
    ;;Loop(en)
    (ntimes en
      ;;uint8
      (rinte in 1 buffer)
      ;;uint16 - cn
      (let ((cn (readin-int in)))
        (inte cn 2 buffer)
        ;;Loop(cn)
        (ntimes cn
          ;;uint16
          (rinte in 2 buffer)
          ;;uint16
          (rinte in 2 buffer))))))

  
;;Anim Layer
;;Contains: uint16
;;          uint16
;;          uint16 - ids
;;          Loop(ids): uint16
(deflayer make-file-anim :data-binary
  ;;uint16
  (rinte in 2 buffer)
  ;;uint16
  (rinte in 2 buffer)
  ;;uint16 - ids
  (let ((ids (readin-int in)))
    (inte ids 2 buffer)
    ;;Loop(ids)
    (ntimes ids
      ;;uint16
      (rinte in 2 buffer))))

;;Tileset Layer
;;Contains: uint8 
;;          uint16 - flnum
;;          uint16
;;          Loop(flnum): String
;;                       uint16
;;                       uint8
(deflayer make-file-tileset :data-binary
  ;;uint8
  (rinte in 1 buffer)
  ;;uint16 - flnum
  (let ((flnum (readin-int in)))
    (inte flnum 2 buffer)
    ;;uint16
    (rinte in 2 buffer)
    ;;Loop(flnum)
    (ntimes flnum
      ;;String
      (rstre in buffer)
      ;;uint16
      (rinte in 2 buffer)
      ;;uint8
      (rinte in 1 buffer))))
    
;;Pagina Layer
;;Contains: String
(deflayer make-file-pagina :data-binary
  ;;String
  (rstre in buffer))

;;Action Layer
;;Contains: String
;;          uint16
;;          String
;;          String
;;          uint16
;;          uint16 - ad
;;          Loop(ad): String
(deflayer make-file-action :data-binary
  ;;String
  (rstre in buffer)
  ;;uint16
  (rinte in 2 buffer)
  ;;String
  (rstre in buffer)
  ;;String
  (rstre in buffer)
  ;;uint16
  (rinte in 2 buffer)
  ;;uint16 - ad
  (let ((ad (readin-int in)))
    (inte ad 2 buffer)
    ;;Loop(ad)
    (ntimes ad
      ;;String
      (rstre in buffer))))

;;Code Layer
;;Contains: String
;;          #.class, binary file
(deflayer make-file-code :data-binary
  ;;String
  (rstre in buffer)
  ;;#.class
  (with-open-file (in (concatenate 'string
                                   in-file
                                   ".class")
                      :direction :input
                      :element-type 'unsigned-byte)
    (let ((seq (make-array (file-length in)
                           :element-type '(unsigned-byte 8))))
      (read-sequence seq in)
      (doarr (byte seq)
        (vector-push-extend byte buffer)))))

;;CodeEntry Layer
;;Contains: loop: uint8 - t
;;                loop t=1: String - en 
;;                          String
;;                          if en=='\0': break
;;                loop t=2: String - ln
;;                          if ln=='\0': break
;;                          uint16
(deflayer make-file-codeentry :data-binary
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
      (t (format t "###ERROR: INVALID CODEENTRY LAYER###~%")))))

;;Audio Layer
;;Contains: #.ogg, binary file
(deflayer make-file-audio :none-binary
  ;;#.ogg
  (with-open-file (in (concatenate 'string
                                   in-file
                                   ".ogg")
                      :direction :input
                      :element-type 'unsigned-byte)
    (let ((seq (make-array (file-length in)
                           :element-type '(unsigned-byte 8))))
      (read-sequence seq in)
      (doarr (byte seq)
        (vector-push-extend byte buffer)))))

;;Midi Layer
;;Contains: #.music, binary file
(deflayer make-file-midi :none-binary
  ;;#.music
  (with-open-file (in (concatenate 'string
                                   in-file
                                   ".music")
                      :direction :input
                      :element-type 'unsigned-byte)
    (let ((seq (make-array (file-length in)
                           :element-type '(unsigned-byte 8))))
      (read-sequence seq in)
      (doarr (byte seq)
        (vector-push-extend byte buffer)))))
