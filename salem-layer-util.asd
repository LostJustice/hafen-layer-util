;;; -*- Lisp -*- mode
(defsystem :salem-layer-util
    :name "salem-layer-util"
    :author "Corey Furmanski"
    :license "MIT"
    :description "A resource utility for Salem"
    :depends-on (:babel :alexandria)
    :components ((:file "packages")
                 ;;globals
                 (:file "const"
                        :depends-on ("packages"))
                 ;;utils
                 (:file "utils/macros" :depends-on ("const"))
                 (:file "utils/io" :depends-on ("utils/macros"))
                 (:file "utils/string" :depends-on ("utils/io"))
                 (:file "utils/int" :depends-on ("utils/io"))
                 (:file "utils/float" :depends-on ("utils/io"))
                 ;;layer
                 (:file "layers/layer" :depends-on ("utils/io"
                                                    "utils/int"
                                                    "utils/string"
                                                    "utils/float"))
                 ;;logic
                 (:file "resource" 
                        :depends-on ("layers/layer"))
                 (:file "salem-layer-util" 
                        :depends-on ("resource"))))
