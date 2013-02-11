(in-package :salem-layer-util)

(defmacro test (&body c)
  `,(test2 c))

(defun test2 (&res c)
  (format t "~A~%" c)
  `(progn (car ,c) ,@c))

(defun handle-lisp (args dhold arg)
  (if (consp arg)
      ;;complex 
      (let ((oargv (dhold-argv dhold)))
        ;;set temp argv
        (setf (dhold-argv dhold) arg)
        ;;push new scope onto data for use
        (push '() (dhold-data dhold))
        (salem-layer-util::while (dhold-argv dhold)
          (parse-logic args dhold (car (dhold-argv dhold)))
          (setf (dhold-argv dhold) (cdr (dhold-argv dhold))))
        ;;reset to older argv
        (setf (dhold-argv dhold) oargv)
        ;;reduce data back a stage
        (let ((cur-data (pop (dhold-data dhold))))
          (setf (car (dhold-data dhold))
                (append (car (dhold-data dhold))
                        (list cur-data)))))
      ;;atoms
      (setf (car (dhold-data dhold)) 
            (append (car (dhold-data dhold)) (list arg)))))
      
(defun parse-logic (args dhold arg)
  (if (eq (car args) :encode)
      (case arg
        ;;;Variables
        (:let (let-encode (second args) (third args) dhold))
        
        ;;;Functions
        ;;ints
        (:int64 
         (int-64-encode (second args) (third args) dhold))
        (:int32 
         (int-32-encode (second args) (third args) dhold))
        (:int16 
         (int-16-encode (second args) (third args) dhold))
        (:int8  
         (int-8-encode (second args) (third args) dhold))
        ;;uints
        (:uint64 
         (uint-64-encode (second args) (third args) dhold))
        (:uint32 
         (uint-32-encode (second args) (third args) dhold))
        (:uint16 
         (uint-16-encode (second args) (third args) dhold))
        (:uint8  
         (uint-8-encode (second args) (third args) dhold))
        ;;float
        (:float   
         (float-encode (second args) (third args) dhold))
        ;;string
        (:string  
         (string-encode (second args) (third args) dhold))
    
        ;;intermixed Lisp code
        (t (handle-lisp args dhold arg))
        )
      (case arg
        ;;;Variables
        (:let (let-decode (second args) (third args) (fourth args) dhold))
        
        ;;;Functions
        ;;ints
        (:int64 
         (int-64-decode (second args) (third args) (fourth args) dhold))
        (:int32 
         (int-32-decode (second args) (third args) (fourth args) dhold))
        (:int16 
         (int-16-decode (second args) (third args) (fourth args) dhold))
        (:int8  
         (int-8-decode (second args) (third args) (fourth args) dhold))
        ;;uints
        (:uint64 
         (uint-64-decode (second args) (third args) (fourth args) dhold))
        (:uint32 
         (uint-32-decode (second args) (third args) (fourth args) dhold))
        (:uint16 
         (uint-16-decode (second args) (third args) (fourth args) dhold))
        (:uint8  
         (uint-8-decode (second args) (third args) (fourth args) dhold))
        ;;float
        (:float   
         (float-decode (second args) (third args) (fourth args) dhold))
        ;;string
        (:string  
         (string-decode (second args) (third args) (fourth args) dhold))
    
        ;;intermixed Lisp code
        (t (handle-lisp args dhold arg)))))



(defmacro decoder ((buf off io) &body body)
  "BUF,OFF,IO refering to their respective symbols, BODY is the DSL code to 
be converted to CL"
  (let ((dhold (make-dhold 
                :data (list `(progn))
                :lflag ()
                :argv body)))
    (salem-layer-util::while (dhold-argv dhold) 
      (parse-logic `(:decode ,buf ,off ,io) 
                           dhold 
                           (car (dhold-argv dhold)))
      (setf (dhold-argv dhold) (cdr (dhold-argv dhold))))
    ;;connect the tree correctly
    (reduce (lambda (inner outer)
              (append outer (list inner)))
            (dhold-data dhold))))

(defmacro encoder ((buffer io) &body body)
  "BUFFER,IO refering to their respective symbols, BODY is the DSL code to be
converted to CL"
  (let ((dhold (make-dhold 
                :data (list `(progn))
                :lflag ()
                :argv body)))
    (salem-layer-util::while (dhold-argv dhold) 
      (parse-logic `(:encode ,buffer ,io) 
                   dhold 
                   (car (dhold-argv dhold)))
      (setf (dhold-argv dhold) (cdr (dhold-argv dhold))))
    ;;connect the tree correctly
    (reduce (lambda (inner outer)
              (append outer (list inner)))
            (dhold-data dhold))))
