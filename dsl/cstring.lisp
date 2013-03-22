(in-package :salem-layer-util)

(defun cstring-encode (buffer io dhold)
  "Transforms an cstring encode into proper CL code"
  (if (dhold-lflag dhold)
      (progn
        (setf (car (dhold-data dhold))
              (append (car (dhold-data dhold))
                      `( ((,(dhold-lflag dhold) (readin-next ,io)))
                         (stre ,(dhold-lflag dhold) ,buffer t) ))) 
        (setf (dhold-lflag dhold) nil))
      (setf (car (dhold-data dhold))
            (append (car (dhold-data dhold))
                    `((rcstre ,io ,buffer))))))

