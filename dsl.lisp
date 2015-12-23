(in-package :hlu)

;; Need support for:
;; uint8,uint16,uint32,uint64
;; int8,int16,int32,int64
;; string
                 :string ((format nil "fln[~A]" i))
                 :uint16 ((format nil "flv[~A]" i))
                 :uint8  ((format nil "flw[~A]" i)))
;; coord
;; color
;; float32
;; float64
;; cpfloat
;; dlist - no args
;; raw [n,:all] fn
;;
;; All arguments for the above are as follwoed unless specified above
;; sym ("comment1" ...)
;; where sym will be repeated for each comment

(defstruct meta
  code
  ret)

(defmacro resolve (sym)
  `(symbol-function (intern (symbol-name ,sym))))



(defun parse-list (itms out)
  (dolist (itm itms)
    (format out ";;Don't edit next line~%")
    (format out "~A~%" (car itm))
    (if (= (car itm) +t-ttol+)
        (parse-list (cdr itm) out)
        (if (= (car itm) +t-bytes+)
            (progn
              (format out ";;Number of bytes~%")
              (format out "~A~%" (length (cdr itm)))
              (format out "~A~%" (cdr itm)))
            (format out "~A~%" (cdr itm)))))
  (format out ";;Don't edit next line~%")
  (format out "LIST_END~%"))

(defun add-code (msg out fun comments)
  (let ((last (gensym)))
    `(progn
       ,@(loop
            for comment in comments
            collect `(let ((,last (funcall ,fun ,msg)))
                       (format ,out ";; ~A~%" ,comment)
                       (format ,out "~A~%" ,last)
                       ,last)))))

(defun parse-block (meta msg out)
  (let ((ret `()))
    (do ((sym (car (meta-code meta)) (car (meta-code meta))))
        ((null sym))
      (case sym
        ((:uint8 :uint16 :uint32 :uint64
                 :int8 :int16 :int32 :int64
                 :coord :color :fcolor
                 :float32 :float64 :cpfloat)
         (setf (meta-code meta) (cdr (meta-code meta)))
         (let ((comment (car (meta-code meta))))
           (when (not (listp comment))
             (setf comment `(,comment)))
           (appendn (add-code msg out (resolve sym) comment)
                    ret)))
        (:string
         (setf (meta-code meta) (cdr (meta-code meta)))
         (let ((comment (car (meta-code meta))))
           (when (not (listp comment))
             (setf comment `(,comment)))
           (appendn (add-code msg out #'dstring comment)
                    ret)))
        (:dlist
         (appendn `(parse-list (dlist ,msg) ,out) ret))
        (:raw
         (let ((mode (cadr (meta-code meta)))
               (fn (caddr (meta-code meta))))
           (if (eq mode :all)
               (appendn `(write-to-file (rest-of-bytes ,msg) ,fn) ret)
               (appendn `(write-to-file (bytes ,msg ,mode) ,fn) ret))
           (setf (meta-code meta) (cdr (cdr (meta-code meta))))))
        (t 
         (if (listp sym)
             (appendn (parse-block (make-meta :code sym) msg out) ret)
             (appendn sym ret))))
      (setf (meta-code meta) (cdr (meta-code meta))))
    `,ret))
  

(defmacro decoder (msg out &body body)
  `(progn
     ,@(parse-block (make-meta :code body)
                    msg
                    out)))

(defun parse-eblock (meta in out)
  (let ((ret `()))
    (do ((sym (car (meta-code meta)) (car (meta-code meta))))
        ((null sym))
      (case sym
        ((:uint8 :uint16 :uint32 :uint64
                 :int8 :int16 :int32 :int64)
         (let ((bytes (/ (parse-integer
                          (subseq (symbol-name sym)
                                  (+ 3 (search "INT" (symbol-name sym)))))
                         8)))
           (appendn `(eint ,out (read-int ,in) ,bytes) ret)))
        (:coord
         (let ((i (gensym)))
           (appendn `(dolist (,i (read-special ,in))
                       (eint ,out ,i 2))
                    ret)))
        (:color
         (let ((i (gensym)))
           (appendn `(dolist (,i (read-special ,in))
                       (eint ,out ,i 1))
                    ret)))
        (:fcolor
         (let ((i (gensym)))
           (appendn `(dolist (,i (read-special ,in))
                       (ecpfloat ,out ,i))
                    ret)))
        (:float32
         (appendn `(efloat32 ,out (read-special ,in)) ret))
        (:float64
         (appendn `(efloat64 ,out (read-special ,in)) ret))
        (:cpfloat
         (appendn `(ecpfloat ,out (read-special ,in)) ret))
        (:string
         (appendn `(estring ,out (read-string ,in)) ret))
        (:dlist
         (appendn `(edlist ,in ,out) ret))
        (:raw
         (let ((fn (cadr (meta-code meta)))
               (fd (gensym))
               (data (gensym)))
           (setf (meta-code meta) (cdr (meta-code meta)))
           (appendn `(with-open-file (,fd ,fn
                                          :element-type '(unsigned-byte 8))
                       (let ((,data (make-array (file-length ,fd)
                                                :element-type '(unsigned-byte 8))))
                         (read-sequence ,data ,fd)
                         (write-sequence ,data ,out)))
                    ret)))
         (t (if (listp sym)
                (appendn (parse-eblock (make-meta :code sym) in out) ret)
                (appendn sym ret))))
      (setf (meta-code meta) (cdr (meta-code meta))))
    `,ret))
        
(defmacro encoder (in out &body body)
  `(progn
     ,@(parse-eblock (make-meta :code body)
                     in
                     out)))
