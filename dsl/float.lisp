(in-package :salem-layer-util)


(defun float-decode (buf off io dhold)
  "Transforms an float decode into proper CL code"
  (let ((arg (car (cdr (dhold-argv dhold)))))
    (if (consp arg)
        ;;multiple args
        (if (dhold-lflag dhold)
            (error "Invalid use of LET keyword")
            ;;add new data
            (setf (car (dhold-data dhold))
                  (append (car (dhold-data dhold))
                          `((setf ,off
                                 (rw-floats ,buf ,off ,io ";~A (float):"
                                            ;;correctly generate lists for
                                            ;;muti arg function
                                            (list 
                                             ,@(loop 
                                                  for a in arg
                                                  collect `(list ,a)))))))))
        ;;singular
        (if (dhold-lflag dhold)
            ;;add new data
            (setf (car (dhold-data dhold))
                  (append (car (dhold-data dhold))
                          `((rw-float ,buf ,off ,io
                                     ,(cat-args 
                                       (car (cdr (dhold-argv dhold))) 
                                       " (float):"))
                            (setf ,off 
                                  ,(car (cdr (cdr (dhold-lflag dhold))))))))
            ;;add enw data
            (setf (car (dhold-data dhold))
                  (append (car (dhold-data dhold))
                          `((setf ,off
                                  (rw-float ,buf ,off ,io
                                            ,(cat-args 
                                              (car (cdr (dhold-argv dhold))) 
                                              " (float):"))))))))
        ;;move argv and reset lflag
    (setf (dhold-argv dhold) (cdr (dhold-argv dhold)))
    (setf (dhold-lflag dhold) ())))

(defun float-encode (buffer io dhold)
  "Transforms an float encode into proper CL code"
  (if (dhold-lflag dhold)
      (progn
        (setf (car (dhold-data dhold))
              (append (car (dhold-data dhold))
                      `( ((,(dhold-lflag dhold) (readin-float ,io)))
                         (floate ,(dhold-lflag dhold) ,buffer) ))) 
        (setf (dhold-lflag dhold) nil))
      (setf (car (dhold-data dhold))
            (append (car (dhold-data dhold))
                    `((rfloate ,io ,buffer))))))

