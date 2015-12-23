(in-package :salem-layer-util)

(defstruct dhold
  data lflag argv)

(defun cat-args (var type)
  (if (symbolp var)
      `(concatenate 'string ";" ,var ,type)
      (concatenate 'string ";" var type)))
