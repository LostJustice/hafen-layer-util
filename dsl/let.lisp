(in-package :salem-layer-util)

(defun let-decode (buf off io dhold)
  (declare (ignore buf off io))
  (let ((var-sym (car (cdr (dhold-argv dhold))))
        (var-arg (car (cdr (cdr (cdr (dhold-argv dhold)))))))
    (when (consp var-sym)
      (error "use normal LET for normal LET features"))
    (if (or (stringp var-arg) (numberp var-arg) (consp var-arg))
        ;;constants
        (progn 
          (push `(let ((,var-sym ,var-arg))) (dhold-data dhold))
          (setf (dhold-argv dhold) (cdr (dhold-argv dhold))))
        (progn
          (let ((noff (gensym)))
            (push `(multiple-value-bind (,noff ,var-sym)) 
                  (dhold-data dhold))
            (setf (dhold-lflag dhold) `(t ,var-sym ,noff)))))
    (setf (dhold-argv dhold) (cdr (cdr (dhold-argv dhold))))))

(defun let-encode (buffer io dhold)
  (let ((var-sym (car (cdr (dhold-argv dhold)))))
    (if (consp var-sym)
        (handle-lisp `(:encode ,buffer ,io) dhold 'let)
        (let ((var-arg (car (cdr (cdr (cdr (dhold-argv dhold)))))))
          (if (or (stringp var-arg) (numberp var-arg) (consp var-arg))
              ;;constants
              (progn
                (push `(let ((,var-sym ,var-arg))) (dhold-data dhold))
                (setf (dhold-argv dhold) (cdr (dhold-argv dhold))))
              (progn
                (push `(let) (dhold-data dhold))
                (setf (dhold-lflag dhold) var-sym)))
          (setf (dhold-argv dhold) (cdr (cdr (dhold-argv dhold))))))))
