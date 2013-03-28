#!/bin/sh
sbcl --eval "(progn (declare (optimize (speed 3))) (ql:quickload \"salem-layer-util\") (load \"build.lisp\"))"
