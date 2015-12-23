(in-package :salem-layer-util)

(defun copy-raw-to-file (buf filename &key (start 0) (end nil))
  "Copys a raw BUFfer to specified FILENAME"
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede
                       :element-type 'unsigned-byte)
    (write-sequence buf out :start start :end end)))

(defun push-file-to-buffer (buf filename)
  "Copys contents of a FILENAME to the BUFfer"
  (with-open-file (in filename
                      :direction :input
                      :element-type 'unsigned-byte)
    (let ((seq (make-array (file-length in)
                           :element-type '(unsigned-byte 8))))
      (read-sequence seq in)
      (doarr (byte seq)
        (vector-push-extend byte buf)))))

(defun buf->arr (buf off len)
  "Makes an array out of select slots in the buffer"
  (let ((arr (make-array len
                         :element-type '(unsigned-byte 8))))
    (do ((i 0 (1+ i)))
        ((>= i len))
      ;;transfer
      (setf (aref arr i) (aref buf (+ off i))))
    arr))

;;;Bianry IO write operations
(defun newline (stream)
  "because terpri doesn't work in binary mode"
  (write-byte 10 stream))

(defun write-str (str stream)
  "Writes a string to a binary stream"
  (write-sequence
   (str->ub str)
   stream)
  (newline stream))

(defun write-all (buffer io)
  "Writes the entire contents of BUFFER to IO"
  (write-sequence buffer io)
  (doarr (i buffer)
    (write-byte i io)))

(defun write-till (buf io off len)
  "Writes a series of bytes from off->len"
  (setf len (+ len off))
  (do ()
      ((> off len))
    (write-byte (aref buf off) io)
    (incf off)))

;;;;0xEF BB BF
(defun remove-bom (io)
  "Reads over the BOM in utf-8 files"
  (when (char= (peek-char nil io nil :eof) (code-char #xFEFF))
    (read-char io)))


;;;Folder input operations
(defun readin-next (io &optional (limit -1))
  "Reads in our next string input from IO-stream and returns it"
  (let ((buf :eof)
        (lines 0))
    (do ((ln (read-line io nil :eof)
             (read-line io nil :eof)))
        ((eq ln :eof))
      ;;non-comments only
      (when (or (zerop (length ln))
                (char/= (char ln 0) #\;))
        (incf lines)
        ;;resolve buf
        (if (stringp buf)
            (setf buf (concatenate 'string buf (string #\NewLine) ln))
            (setf buf ln))
        
        ;;kill early if next line is comment or EOF or if hit limit
        (when (or (= lines limit) 
                  (eq (peek-char nil io nil :eof) :eof)
                  (char= (peek-char nil io nil :eof) #\;))
          (return-from readin-next buf))))
    buf))

(defun readin-any (io)
  "Reads in the next valid line of any type from IO-stream"
  (read-from-string (readin-next io 1)))
