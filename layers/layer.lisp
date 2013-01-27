(in-package :salem-layer-util)

;;structs
(defstruct layer
  (name nil :type symbol)
  (sname "" :type string)
  (id 0 :type number)
  (decode-func nil :type symbol)
  (encode-func nil :type symbol))

;;globals
(defparameter *layers-table* (make-hash-table))

;;macros
(defmacro generate-type (name fnsym type args body)
  "Not for normal use outside of deflayer"
  (case type
    ;;NA
    (:defundef `(defun ,fnsym () (format t "Undefined Function ~A~%" ',name)))
    
    ;;DECODING
    (:defnone
     `(defun ,fnsym (,(first args) ,(second args))
        (when *verbose*
          (format t "   Layer: ~A~%" ',name)
          (format t "   Len  : ~A~%" (length ,(first args))))
        ,@body))
    (:defdata
     `(defun ,fnsym (,(first args) ,(second args))
        (when *verbose*
          (format t "   Layer: ~A~%" ',name)
          (format t "   Len  : ~A~%" (length ,(first args))))
        (let ((,(third args) 0))
          (with-open-file (,(fourth args) (concatenate 'string
                                                       io
                                                       ".data")
                            :direction :output
                            :if-exists :supersede)
            ,@body))))
    
    ;;ENCODING
    (:defnone-binary
     `(defun ,fnsym (,(first args) ,(second args))
        (when *verbose*
          (format t "   Layer: ~A~%" ',name))
        (let ((,(third args) (make-array 1 
                                         :element-type '(unsigned-byte 8)
                                         :adjustable t 
                                         :fill-pointer 0)))
          ,@body
          ;;write data
          (when *verbose*
            (format t "   Len  : ~A~%" (length ,(third args))))
          (write-sequence (int->ubarr (length ,(third args)) 4) ,(second args))
          (write-sequence ,(third args) ,(second args)))))
    (:defdata-binary
     `(defun ,fnsym (,(first args) ,(second args))
        (when *verbose*
          (format t "   Layer: ~A~%" ',name))
        (let ((,(third args) (make-array 1 
                                         :element-type '(unsigned-byte 8)
                                         :adjustable t 
                                         :fill-pointer 0)))
          (with-open-file (,(fourth args) (concatenate 'string
                                                       in-file
                                                       ".data")
                            :direction :input)
            (remove-bom ,(fourth args))
            ,@body)
          ;;write data
          (when *verbose*
            (format t "   Len  : ~A~%" (length ,(third args))))
          (write-sequence (int->ubarr (length ,(third args)) 4) ,(second args))
          (write-sequence ,(third args) ,(second args)))))

    (t (error (format nil "Invalid type ~A~%" name)))))


(let ((ind 0))
  (defmacro deflayer (name () 
                      (decode-type (&rest dargs) () &body dbody)
                      (encode-type (&rest eargs) () &body ebody))
    "Generates a layer definition based on the arguments.
Decoding-type and Encoding-type and args/body refer to the following:
Note: elements of args are to be symbols for variables
Types:
:defundef -> undefined type functions
:defnone  -> basic decoding type function
- Arguments expected:
1. incoming array-buffer [buf]
2. outbound file name [io]
:defdata  -> :none + automatically opens up a .data file for writing
- Arguments expected:
1/2 All of the ones from :none
3. offset counter [off]
4. .data file handler [out]
:defnone-binary -> the basic encoding type function
- Arguments expected:
1. Incoming file name [in-file]
2. Write-only file stream [io]
3. array-buffer [buffer]
:defdata-binary -> :none-binary + automatically opens up .data file for reading
- Arguments expected:
1-3 Same as :none-binary
4. data file handler [in]"
    (let* ((dfunc (alexandria:symbolicate name '-decode))
           (efunc (alexandria:symbolicate name '-encode))
           (layer (make-layer :name name
                              :sname (string-downcase (symbol-name name))
                              :id ind
                              :decode-func dfunc
                              :encode-func efunc))
           (data `(progn
                    (generate-type ,name ,dfunc ,decode-type ,dargs ,dbody)
                    (generate-type ,name ,efunc ,encode-type ,eargs ,ebody))))
      (incf ind)
      (setf (gethash name *layers-table*) layer)
      data)))



#|Ex layer
(deflayer test () 
  (:none (buf io) ()
    (+ 1 2)...)
  (:data-binary (in-file io buffer in) ()
     ...))
|#
