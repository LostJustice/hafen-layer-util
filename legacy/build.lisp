;;sbcl errors about hlu not existing if it's part of the --eval line when compiling
#+win32
(sb-ext:save-lisp-and-die "salem-layer-util.exe" :toplevel #'hlu::bootstrap :executable t :purify t)
#+(not win32)
(sb-ext:save-lisp-and-die "salem-layer-util" :toplevel #'slu::bootstrap :executable t :purify t)
