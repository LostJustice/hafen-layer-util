;;;; 

(in-package :salem-layer-util)

(defconstant +resource-sig+ "Haven Resource 1")
(defconstant +resource-sig-octets+ (string-to-octets +resource-sig+))
(defconstant +version-size+ 2) ;bytes
(defconstant +layer-len-size+ 4) ;bytes
(defparameter *file-layer-callback*
  ;"Callback functions for the various layers of a resource file"
  `(("vbuf"      . (,#'make-fold-vbuf      . 0 ))
    ("mesh"      . (,#'make-fold-mesh      . 1 ))
    ("mat"       . (,#'make-fold-mat       . 2 ))
    ("skel"      . (,#'make-fold-skel      . 3 ))
    ("skan"      . (,#'make-fold-skan      . 4 ))
    ("boneoff"   . (,#'make-fold-boneoff   . 5 ))
    ("light"     . (,#'make-fold-light     . 6 ))
    ("rlink"     . (,#'make-fold-rlink     . 7 ))
    ("image"     . (,#'make-fold-image     . 8 ))
    ("tooltip"   . (,#'make-fold-tooltip   . 9 ))
    ("tile"      . (,#'make-fold-tile      . 10))
    ("neg"       . (,#'make-fold-neg       . 11))
    ("anim"      . (,#'make-fold-anim      . 12))
    ("tileset"   . (,#'make-fold-tileset   . 13))
    ("pagina"    . (,#'make-fold-pagina    . 14))
    ("action"    . (,#'make-fold-action    . 15))
    ("code"      . (,#'make-fold-code      . 16))
    ("codeentry" . (,#'make-fold-codeentry . 17))
    ("audio"     . (,#'make-fold-audio     . 18))
    ("midi"      . (,#'make-fold-midi      . 19))))
(defparameter *fold-layer-callback*
  ;"Callback functions for the various layers of a folder format"
  `(("vbuf"      . (,#'make-file-vbuf      . 0 ))
    ("mesh"      . (,#'make-file-mesh      . 1 ))
    ("mat"       . (,#'make-file-mat       . 2 ))
    ("skel"      . (,#'make-file-skel      . 3 ))
    ("skan"      . (,#'make-file-skan      . 4 ))
    ("boneoff"   . (,#'make-file-boneoff   . 5 ))
    ("light"     . (,#'make-file-light     . 6 ))
    ("rlink"     . (,#'make-file-rlink     . 7 ))
    ("image"     . (,#'make-file-image     . 8 ))
    ("tooltip"   . (,#'make-file-tooltip   . 9 ))
    ("tile"      . (,#'make-file-tile      . 10))
    ("neg"       . (,#'make-file-neg       . 11))
    ("anim"      . (,#'make-file-anim      . 12))
    ("tileset"   . (,#'make-file-tileset   . 13))
    ("pagina"    . (,#'make-file-pagina    . 14))
    ("action"    . (,#'make-file-action    . 15))
    ("code"      . (,#'make-file-code      . 16))
    ("codeentry" . (,#'make-file-codeentry . 17))
    ("audio"     . (,#'make-file-audio     . 18))
    ("midi"      . (,#'make-file-midi      . 19))))

(defun read-times (io n)
  "Read in N bytes from IO stream"
  (let ((buf (make-array n :element-type '(unsigned-byte 8))))
    (dotimes (i n)
      (setf (aref buf i) (read-byte io nil -1)))
    buf))

(defun load-resource-by-res (in-file out-folder)
  "Read in a resource by .res/.cache file"
  (ensure-directories-exist out-folder)
  (with-open-file (in in-file 
                      :direction :input
                      :element-type 'unsigned-byte)
    (when *verbose*
      (format t " Scanning ~A~%" in-file))
    ;;Check for HNH/Salem file sig, abort if not valid
    (when (string/= +resource-sig+ (octets-to-string 
                                    (read-times in (length 
                                                    +resource-sig-octets+))))
      (format t " ~A is not a valid resource file.~%" in-file)
      (return-from load-resource-by-res nil))
    ;;Get resource file version & store in meta file
    (with-open-file (io-meta (concatenate 'string
                                          out-folder
                                          "meta")
                             :direction :output
                             :if-exists :supersede)
      (let ((ver (ubarr->uint (read-times in +version-size+))))
        (when *verbose*
          (format t " Version: ~A~%" ver))
        (format io-meta ";Meta file for resource file ~A~%" in-file)
        (write-line ";Version [ver]:[unsigned-int-16]" io-meta)
        (write-line (write-to-string ver) io-meta)))
    ;;Scan for layers
    (let ((sbuf "")
          (lc (make-array (length *file-layer-callback*)
                          :initial-element 0)))
      (do ((i (read-byte in nil -1)
              (read-byte in nil -1)))
          ((minusp i))
        (if (= i 0)
            ;;found string
            (progn
              (when *verbose*
                (format t " String: ~A~%" sbuf))
              (let ((layer (assoc sbuf *file-layer-callback* :test #'string=)))
                ;;check if layer
                (when (not (null layer))
                  ;;get size of layer
                  (let ((len (ubarr->uint (read-times in +layer-len-size+)))
                        (layern (concatenate 'string
                                             out-folder
                                             (car layer)
                                             "/"
                                             (write-to-string (aref lc (cddr layer))))))
                    ;;make layer directory if needed
                    (ensure-directories-exist layern)
                    ;;make layer
                    (when *verbose*
                      (format t "  Scanning layer...~%"))
                    (funcall (car (cdr layer)) (read-times in len) layern)))
                    ;;inc layer count
                    (incf (aref lc (cddr layer))))
              (setf sbuf ""))
            ;;continue building string
            (setf sbuf (concatenate 'string
                                   sbuf
                                   (string (code-char i)))))))))

(defun rmetae (base-file io)
  "Read the meta file and encode it"
  (with-open-file (in (concatenate 'string (namestring base-file) "/meta"))
    ;;uint8 Version
    (write-sequence (int->ubarr (readin-int in) 2) io)))

(defun solve-high (layer)
  ;;get all files
  (let ((files (directory (make-pathname :directory (pathname-directory layer)
                                         :name :wild
                                         :type :wild)))
        (high 0))
    (dolist (f files)
      (let ((num (parse-integer (pathname-name f) :junk-allowed t)))
        ;;only #'s accepted
        (when (and (not (null num))
                   (> num high))
          (setf high num))))
    high))
    

(defun load-resource-by-folder (folder outfile)
  "Read in a resource from a slayer-util produced folder"
  (ensure-directories-exist outfile)
  (with-open-file (out-io outfile :direction :output
                          :if-exists :supersede
                          :element-type 'unsigned-byte)
    ;;Write hnh/salem sig
    (write-sequence +resource-sig-octets+ out-io)
    ;;parse meta
    (rmetae folder out-io)
    ;;get all layers
    (let ((layers (directory (make-pathname :directory (pathname-directory folder)
                                            :name :wild
                                            :type :wild))))
      ;;encode them all
      (dolist (layer layers)
        ;;layers are directories
        (let ((layer-cb (assoc (car (last (pathname-directory layer)))
                               *fold-layer-callback* :test #'string=)))
          (when (and (null (pathname-type layer))
                     (null (pathname-name layer))
                     (not (null layer-cb)))
            ;;encode data 
            (dotimes (i (1+ (solve-high layer)))
              (when *verbose*
                (format t "  Encoding Layer[~A#~A]...~%" (car layer-cb) i))
              ;;encode layer name
              (write-sequence (str->ubarr (car layer-cb)) out-io)
              ;;encode its data
              (funcall (car (cdr layer-cb)) 
                       (concatenate 'string
                                    (namestring layer)
                                    "/"
                                    (write-to-string i))
                       out-io))))))))
