(in-package :salem-layer-util)
;;;General
(defmacro doarr ((var array &optional (result nil)) &body body)
  "dolist, but for arrays"
  (let ((inc (gensym)))
    `(let ((,var 0))
       (do ((,inc 0 (1+ ,inc)))
           ((>= ,inc (length ,array)) ,result)
         (setf ,var (aref ,array ,inc))
         ,@body))))

(defmacro ntimes (n &body body)
  "dotimes without worrying about the variable"
  `(dotimes (,(gensym) ,n)
     ,@body))

(defmacro 2* (num)
  `(* ,num 2))

(defmacro 3* (num)
  `(* ,num 3))

(defmacro while (test-expr &body body)
  `(do ()
       ((not ,test-expr))
     ,@body))
