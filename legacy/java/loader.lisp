(require 'asdf)

(progn
  (declaim (optimize (speed 3)))
  (push
   (make-pathname :device (pathname-device *default-pathname-defaults*)
                  :directory (append (pathname-directory *default-pathname-defaults*)
                                     (list "java" "alexandria-20130128-git")))
   asdf:*central-registry*)
  
  (asdf:operate 'asdf:load-op 'alexandria)
  (asdf:operate 'asdf:load-op 'salem-layer-util))
