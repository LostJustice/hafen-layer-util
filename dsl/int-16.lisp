(in-package :salem-layer-util-dsl)

(defun int-16-decode (buf off io dhold)
  "Transforms an int-16 decode into proper CL code"
  (let ((arg (car (cdr (dhold-argv dhold)))))
    (if (consp arg)
        ;;multiple args
        (if (dhold-lflag dhold)
            (error "Invalid use of LET keyword")
            ;;add new data
            (setf (car (dhold-data dhold))
                  (append (car (dhold-data dhold))
                          `((setf ,off 
                                  (rw-sints ,buf ,off 2 ,io ";~A (int-16):"
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
                          `((rw-sint ,buf ,off 2 ,io
                                     ,(cat-args 
                                       (car (cdr (dhold-argv dhold))) 
                                       " (int-16):"))
                            (setf ,off 
                                  ,(car (cdr (cdr (dhold-lflag dhold))))))))
            ;;add enw data
            (setf (car (dhold-data dhold))
                  (append (car (dhold-data dhold))
                          `((setf ,off 
                                  (rw-sint ,buf ,off 2 ,io
                                           ,(cat-args 
                                             (car (cdr (dhold-argv dhold))) 
                                             " (int-16):"))))))))
    ;;move argv and reset lflag
    (setf (dhold-argv dhold) (cdr (dhold-argv dhold)))
    (setf (dhold-lflag dhold) ())))

(defun int-16-encode (buffer io dhold)
  "Transforms an int-16 encode into proper CL code"
  (if (dhold-lflag dhold)
      (error "Use of LET in encode mode does not work on functions")
      (setf (car (dhold-data dhold))
            (append (car (dhold-data dhold))
                    `((rinte ,io 2 ,buffer))))))

