;;;; hafen-layerutil.asd

(asdf:defsystem #:hafen-layerutil
  :description "Hafen Resource Layer Util"
  :author "Corey Furmanski"
  :license "MIT"
  :depends-on (#:alexandria
               #:sb-concurrency
               #:ieee-floats
               #:flexi-streams)
  :serial t
  :components ((:file "package")
               (:file "macros" :depends-on ("package"))
               (:file "utf" :depends-on ("package macros"))
               (:file "message" :depends-on ("utf"))
               (:file "reader" :depends-on ("utf"))
               (:file "dsl" :depends-on ("message"))
               
               (:file "iotask" :depends-on ("package"))
               (:file "layer" :depends-on ("macros"))
               (:file "resource" :depends-on ("layer"))

               

               (:file "hafen-layerutil"
                      :depends-on ("iotask"
                                   "resource"))))

