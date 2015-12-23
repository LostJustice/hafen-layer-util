(in-package :salem-layer-util)

(deflayer vbuf ()
  ;;VertexRes Layer
  ;;uint8 - fl
  ;;uint16 - num
  ;;while(off < buf.length)
  ;;  uint8 - id
  ;;  (id = 0):
  ;;    for 1...(num*3):
  ;;      float - vertices
  ;;  (id = 1):
  ;;    for 1...(num*3)
  ;;      float - normals
  ;;  (id = 2):
  ;;    for 1...(num*2)
  ;;      float - texels
  ;;  (id = 3):
  ;;    uint8 - mba
  ;;    while(t):
  ;;      cstring - bone
  ;;      (bone = ""): BREAK
  ;;      while(t):
  ;;        uint16 - run
  ;;        uint16 - st
  ;;        (run = 0): BREAK
  ;;        for 0...run:
  ;;          float - w
  (:defdata (buf io off out) ()
    :uint8 "fl"
    :let num = :uint16 "num"
    (while (< off (length buf))
      :let id = :uint8 "id"
      (case id
        (0 (dotimes (i (* num 3))
             :float ((format nil "vertex(~A)" i))))
        (1 (dotimes (i (* num 3))
             :float ((format nil "normal(~A)" i))))
        (2 (dotimes (i (* num 2))
             :float ((format nil "texel(~A)" i))))
        (3 :uint8 "mba"
           (let ((stop0 nil))
             (while (null stop0)
               :let bone = :cstring "bone"
               (if (string= bone "")
                   (setf stop0 t)
                   (let ((stop1 nil))
                     (while (null stop1)
                       :let run = :uint16 "run"
                       :uint16 "st"
                       (if (zerop run)
                           (setf stop1 t)
                           (dotimes (i run)
                             :float ((format nil "w(~A)" i))))))))))
        (t (error "vbuf-layer: invalid id")))))
  (:defdata-binary (in-file io buffer in) ()
    :uint8
    :let num = :uint16
    (do ((id (readin-int in)
             (readin-int in)))
        ((eq id :eof))
      (inte id 1 buffer)
      (case id
        (0 (ntimes (* num 3) :float))
        (1 (ntimes (* num 3) :float))
        (2 (ntimes (* num 2) :float))
        (3 :uint8 
           (let ((stop0 nil))
             (while (null stop0)
               :let bone = :cstring 
               (if (string= bone "")
                   (setf stop0 t)
                   (let ((stop1 nil))
                     (while (null stop1)
                       :let run = :uint16
                       :uint16
                       (if (zerop run)
                           (setf stop1 t)
                           (ntimes run :float))))))))
        (t (error "vbuf-layer: invalid id"))))))
