(in-package :salem-layer-util)


(defun string-decode (buf off io dhold)
  "Transforms an string decode into proper CL code"
  (let ((arg (car (cdr (dhold-argv dhold)))))
    (if (consp arg)
        ;;multiple args
        (if (dhold-lflag dhold)
            (error "Invalid use of LET keyword")
            ;;add new data
            (setf (car (dhold-data dhold))
                  (append (car (dhold-data dhold))
                          `((setf ,off (rw-strs ,buf ,off ,io ";~A (string):"
                                      ;;correctly generate lists for multi
                                      ;;arg function
                                      (list ,@(loop 
                                                 for a in arg
                                                 collect `(list ,a)))))))))
        ;;singular
        (if (dhold-lflag dhold)
            ;;add new data
            (setf (car (dhold-data dhold))
                  (append (car (dhold-data dhold))
                          `((rw-str ,buf ,off ,io
                                     ,(cat-args 
                                       (car (cdr (dhold-argv dhold))) 
                                       " (string):"))
                            (setf ,off 
                                  ,(car (cdr (cdr (dhold-lflag dhold))))))))
            ;;add enw data
            (setf (car (dhold-data dhold))
                  (append (car (dhold-data dhold))
                          `((setf ,off (rw-str ,buf ,off ,io
                                     ,(cat-args 
                                       (car (cdr (dhold-argv dhold))) 
                                       " (string):"))))))))
    ;;move argv and reset lflag
    (setf (dhold-argv dhold) (cdr (dhold-argv dhold)))
    (setf (dhold-lflag dhold) ())))

(defun string-encode (buffer io dhold)
  "Transforms an string encode into proper CL code"
  (if (dhold-lflag dhold)
      (progn
        (setf (car (dhold-data dhold))
              (append (car (dhold-data dhold))
                      `( ((,(dhold-lflag dhold) (readin-next ,io)))
                         (stre ,(dhold-lflag dhold) ,buffer) ))) 
        (setf (dhold-lflag dhold) nil))
      (setf (car (dhold-data dhold))
            (append (car (dhold-data dhold))
                    `((rstre ,io ,buffer))))))

