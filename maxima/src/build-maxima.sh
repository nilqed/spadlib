#!/bin/bash
# kfp/17-MAY-2021
MAXIMA=maxima
cd $MAXIMA
#
sbcl --eval "(progn (load \"configure.lisp\") (configure :interactive nil) (quit))"
cd src
sbcl --eval "(progn (load \"maxima-build.lisp\") (maxima-compile) (quit))"
cd ../..
echo $MAXIMA build completed.
echo run MAXIMA by maxima.sh
