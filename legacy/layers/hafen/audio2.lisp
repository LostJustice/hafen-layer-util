(in-package :hlu)

(deflayer audio2 ()
  ;;uint8 - ver
  ;;ver = 1 | ver = 2:
  ;;  cstring - id
  ;;  ver = 2:
  ;;    uint16 - bvol /1000
  ;;  bytes - .ogg
  (:defnone (buf io) ()
    :let ver :uint8 "verison"
    (when (or (= ver 1)
              (= ver 2))
      :cstring "id"
      (when (= ver 2)
        :uint16 "bvol/1000")
      (copy-raw-to-file buf (concatenate 'string
                                         io
                                         ".ogg"))))
  (:defnone-binary (in-file io buffer) ()
    :let ver :uint8
    (if (or (= ver 1)
            (= ver 2))
        (progn
          :cstring
          (when (= ver 2)
            :uint16)
          (push-file-to-buffer buffer
                               (concatenate 'string
                                            in-file
                                            ".ogg")))
        (error "Wrong version given, must be 1 or 2"))))
