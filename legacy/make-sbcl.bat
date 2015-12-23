@echo off
sbcl --eval "(progn (ql:quickload \"salem-layer-util\") (load \"build.lisp\"))"
