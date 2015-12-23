;;; -*- Lisp -*- mode
(defsystem :hafen-layer-util
    :name "hafen-layer-util"
    :author "Corey Furmanski"
    :license "MIT"
    :description "A general purpose layered-file resource utility"
    :depends-on (:alexandria)
    :components ((:file "packages")
                 (:file "utils/utf" :depends-on ("packages"))
                 ;;globals
                 (:file "const"
                        :depends-on ("utils/utf"))
                 ;;utils
                 (:file "utils/macros" :depends-on ("const"))
                 (:file "utils/io" :depends-on ("utils/macros"))
                 (:file "utils/string" :depends-on ("utils/io"))
                 (:file "utils/int" :depends-on ("utils/io"))
                 (:file "utils/float" :depends-on ("utils/io"))
                 ;;dsl code
                 (:file "dsl/common" :depends-on ("packages"))
                 (:file "dsl/int-8" :depends-on ("dsl/common"))
                 (:file "dsl/int-16" :depends-on ("dsl/common"))
                 (:file "dsl/int-32" :depends-on ("dsl/common"))
                 (:file "dsl/int-64" :depends-on ("dsl/common"))
                 (:file "dsl/uint-8" :depends-on ("dsl/common"))
                 (:file "dsl/uint-16" :depends-on ("dsl/common"))
                 (:file "dsl/uint-32" :depends-on ("dsl/common"))
                 (:file "dsl/uint-64" :depends-on ("dsl/common"))
                 (:file "dsl/float" :depends-on ("dsl/common"))
                 (:file "dsl/string" :depends-on ("dsl/common"))
                 (:file "dsl/cstring" :depends-on ("dsl/common"))
                 (:file "dsl/let" :depends-on ("dsl/common"))
                 (:file "dsl/dsl" :depends-on ("utils/float"
                                               "utils/int"
                                               "utils/string"
                                               "dsl/common"
                                               "dsl/int-8"
                                               "dsl/int-16"
                                               "dsl/int-32"
                                               "dsl/int-64"
                                               "dsl/uint-8"
                                               "dsl/uint-16"
                                               "dsl/uint-32"
                                               "dsl/uint-64"
                                               "dsl/float"
                                               "dsl/string"
                                               "dsl/let"))
                 ;;layer
                 (:file "layers/layer" :depends-on ("utils/io"
                                                    "utils/int"
                                                    "utils/string"
                                                    "utils/float"
                                                    "dsl/dsl"))
                 (:file "layers/unknown" :depends-on ("layers/layer"))
                 
                 ;;logic
                 (:file "resource" 
                        :depends-on ("layers/layer" "layers/unknown"))
                 (:file "hnh-neg-fix"
                        :depends-on ("resource"))
                 (:file "hafen-layer-util" 
                        :depends-on ("resource"))))
