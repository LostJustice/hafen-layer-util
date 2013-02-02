(in-package :salem-layer-util)


(defun uint-8-decode (buf off io dhold)
  "Transforms an uint-8 decode into proper CL code"
  (let ((arg (car (cdr (dhold-argv dhold)))))
    (if (consp arg)
        ;;multiple args
        (if (dhold-lflag dhold)
            (error "Invalid use of LET keyword")
            ;;add new data
            (setf (car (dhold-data dhold))
                  (append (car (dhold-data dhold))
                          `((setf ,off 
                                  (rw-uints ,buf ,off 1 ,io ";~A (uint-8):"
                                            ;;correctly generate lists for
                                            ;;multi arg function
                                            (list 
                                             ,@(loop 
                                                  for a in arg
                                                  collect `(list ,a)))))))))
        ;;singular
        (if (dhold-lflag dhold)
            ;;add new data
            (setf (car (dhold-data dhold))
                  (append (car (dhold-data dhold))
                          `((rw-uint ,buf ,off 1 ,io
                                     ,(cat-args 
                                       (car (cdr (dhold-argv dhold))) 
                                       " (uint-8):"))
                            (setf ,off 
                                  ,(car (cdr (cdr (dhold-lflag dhold))))))))
            ;;add enw data
            (setf (car (dhold-data dhold))
                  (append (car (dhold-data dhold))
                          `((setf ,off 
                                  (rw-uint ,buf ,off 1 ,io
                                           ,(cat-args 
                                             (car (cdr (dhold-argv dhold))) 
                                             " (uint-8):"))))))))
    ;;move argv and reset lflag
    (setf (dhold-argv dhold) (cdr (dhold-argv dhold)))
    (setf (dhold-lflag dhold) ())))

(defun uint-8-encode (buffer io dhold)
  "Transforms an uint-8 encode into proper CL code"
  (if (dhold-lflag dhold)
      (error "Use of LET in encode mode does not work on functions")
      (setf (car (dhold-data dhold))
            (append (car (dhold-data dhold))
                    `((rinte ,io 1 ,buffer))))))

