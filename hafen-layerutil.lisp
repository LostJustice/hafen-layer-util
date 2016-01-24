;;;; hafen-layerutil.lisp

(in-package #:hafen-layerutil)

(defun collect-files (folder)
  (let ((files (directory folder))
        (lst ()))
    (dolist (f files)
      (let ((type (pathname-type f))
            (name (car (last (pathname-directory f)))))
        (cond
          ((or (string= type "res")
               (string= type "cached")
               (not (null (or (search ".res" name)
                              (search ".cached" name)))))
           (push f lst))
          ((null (pathname-name f))
           (setf lst
                 (append (collect-files
                          (make-pathname :directory (pathname-directory f)
                                         :name :wild
                                         :type :wild))
                         lst))))))
    lst))

(defun load-layers (lfold)
  (let ((files (directory
                (make-pathname :directory `(:relative ,lfold)
                               :name :wild
                               :type "lisp"))))
    (dolist (f files)
      (load f))))

(defun run (mode in-folder out-folder layers)
  (let ((files (collect-files (make-pathname :directory `(:relative ,in-folder)
                                             :name :wild
                                             :type :wild))))
    (setf in-folder
          (car (directory (make-pathname :directory `(:relative ,in-folder)))))
    (load-layers layers)
    (setf *io-tasks* (sb-concurrency:make-mailbox :name "io-tasks")
          *worker-tasks* (sb-concurrency:make-mailbox :name "worker-tasks"))
    (setf *expected* (length files))

    (if (eq mode :encode)
        (progn
          (setf *saw* 0)
          (dolist (f files)
            (sb-concurrency:send-message *worker-tasks* f))
          (ntimes 3
            (sb-thread:make-thread
             #'worker-encode-fun
             :arguments `(,out-folder ,in-folder)))
          (worker-encode-fun out-folder in-folder))
        (progn
          ;;spawn readers
          (ntimes 4
            (sb-thread:make-thread #'io-read-fun :name "io-reader"))
          (dolist (f files)
            (sb-concurrency:send-message *io-tasks* f))
          (setf *saw* 0)
          ;;spawn de/encoders
          (ntimes 3
            (sb-thread:make-thread
             #'worker-decode-fun
             :arguments `(,out-folder ,in-folder)))
          ;;master becomes a decoder
          (worker-decode-fun out-folder in-folder)))))


(defun bootstrap ()
  (run (intern (nth 1 sb-ext:*posix-argv*) :keyword)
       (nth 2 sb-ext:*posix-argv*)
       (nth 3 sb-ext:*posix-argv*)
       (nth 4 sb-ext:*posix-argv*))
  (sb-ext:exit :code 0))
