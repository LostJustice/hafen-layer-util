(in-package :salem-layer-util-dsl)

(defun let-decode (buf off io dhold)
  (let ((var-sym (car (cdr (dhold-argv dhold)))))
    (if (consp var-sym)
        (handle-lisp `(:decode ,buf ,off ,io) dhold 'let)
        (let ((var-arg (car (cdr (cdr (cdr (dhold-argv dhold)))))))
          (if (or (stringp var-arg) (numberp var-arg) (consp var-arg))
              ;;constants
              (push `(let ((,var-sym ,var-arg))) (dhold-data dhold))
              (progn
                (let ((noff (gensym)))
                  (push `(multiple-value-bind (,noff ,var-sym)) 
                        (dhold-data dhold))
                  (setf (dhold-lflag dhold) `(t ,var-sym ,noff)))))
          (setf (dhold-argv dhold) (cdr (cdr (dhold-argv dhold))))))))

(defun let-encode (buffer io dhold)
  (let ((var-sym (car (cdr (dhold-argv dhold)))))
    (if (consp var-sym)
        (handle-lisp `(:encode ,buffer ,io) dhold 'let)
        (let ((var-arg (car (cdr (cdr (cdr (dhold-argv dhold)))))))
          (if (or (stringp var-arg) (numberp var-arg) (consp var-arg))
              ;;constants
              (push `(let ((,var-sym ,var-arg))) (dhold-data dhold))
              (error "Let is not supported for getting function values
in encode mode"))
          (setf (dhold-argv dhold) (cdr (cdr (dhold-argv dhold))))))))
