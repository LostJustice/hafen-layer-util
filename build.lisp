;;sbcl errors abotu slu not existing if it's part of the --eval line when compiling
#+win32
(sb-ext:save-lisp-and-die "salem-layer-util.exe" :toplevel #'slu::bootstrap :executable t :purify t)
#+(not win32)
(sb-ext:save-lisp-and-die "salem-layer-util" :toplevel #'slu::bootstrap :executable t :purify t)
