(in-package :salem-layer-util)

(defparameter *version-size* 2) ;bytes
(defparameter *layer-len-size* 4) ;bytes

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
    (when (string/= *resource-sig* (octets-to-string 
                                    (read-times in (length 
                                                    *resource-sig-octets*))))
      (format t " ~A is not a valid resource file.~%" in-file)
      (return-from load-resource-by-res nil))
    ;;Get resource file version & store in meta file
    (with-open-file (io-meta (concatenate 'string
                                          out-folder
                                          "meta")
                             :direction :output
                             :if-exists :supersede)
      (let ((ver (ubarr->uint (read-times in *version-size*))))
        (when *verbose*
          (format t " Version: ~A~%" ver))
        (format io-meta ";Meta file for resource file ~A~%" in-file)
        (write-line ";Version [ver]:[unsigned-int-16]" io-meta)
        (write-line (write-to-string ver) io-meta)))
    ;;Scan for layers
    (let ((sbuf "")
          (lc (make-array (hash-table-count *layers-table*)
                          :initial-element 0))
          (unk 0))
      (do ((i (read-byte in nil -1)
              (read-byte in nil -1)))
          ((minusp i))
        (if (= i 0)
            ;;found string
            (progn
              (when *verbose*
                (format t " String: ~A~%" sbuf))
              (let ((layer (gethash (intern (nstring-upcase sbuf)) *layers-table*))
                    (len (ubarr->uint (read-times in *layer-len-size*))))
                ;;check if layer
                (if (layer-p layer)
                    ;;decode known layer
                    (let ((layern (concatenate 'string
                                               out-folder
                                               (layer-sname layer)
                                               "/"
                                               (write-to-string (aref lc (layer-id layer))))))
                      ;;make layer directory if needed
                      (ensure-directories-exist layern)
                      ;;make layer
                      (when *verbose*
                        (format t "  Scanning layer...~%"))
                      (funcall (symbol-function (layer-decode-func layer)) (read-times in len) layern)
                      ;;inc layer count
                      (incf (aref lc (layer-id layer))))
                    ;;decode unknown layer
                    (let ((layern (concatenate 'string out-folder "/" (write-to-string unk))))
                      (incf unk)
                      ;;make directories if needed
                      (ensure-directories-exist layern)
                      (when *verbose*
                        (format t "  Scanning unknown layer...~%"))
                      (unknown-layer-decode (read-times in len) layern)))
                (setf sbuf "")))
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
    (write-sequence *resource-sig-octets* out-io)
    ;;parse meta
    (rmetae folder out-io)
    ;;get all layers
    (let ((layers (directory (make-pathname :directory (pathname-directory folder)
                                            :name :wild
                                            :type :wild))))
      ;;encode them all
      (dolist (layer layers)
        ;;layers are directories
        (let ((layer-cb (gethash (intern (nstring-upcase (car (last (pathname-directory layer)))))
                                 *layers-table*)))
          (when (and (null (pathname-type layer))
                     (null (pathname-name layer))
                     (not (null layer-cb)))
            ;;encode data 
            (dotimes (i (1+ (solve-high layer)))
              (when *verbose*
                (format t "  Encoding Layer[~A#~A]...~%" (layer-name layer-cb) i))
              ;;encode layer name
              (write-sequence (str->ubarr (layer-sname layer-cb)) out-io)
              ;;encode its data
              (funcall (symbol-function (layer-encode-func layer-cb))
                       (concatenate 'string
                                    (namestring layer)
                                    "/"
                                    (write-to-string i))
                       out-io))))))))
