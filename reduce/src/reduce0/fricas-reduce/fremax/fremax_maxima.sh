#!/bin/bash
maxsrc="maxima-code"
maxgit="https://git.code.sf.net/p/maxima/code"

#Get Maxima if necessary
if [ ! -d "$maxsrc" ]; then
  git clone $maxgit $maxsrc
fi


lisp=$PWD/bin/fremax1
cd $maxsrc
$lisp --load "configure.lisp" --eval "(progn (configure :interactive nil)(quit))"

cd src
$lisp --load "maxima-build.lisp" --eval "(progn (maxima-compile)(quit))"

final='(progn (maxima-load) (sb-ext:save-lisp-and-die "fremax2" :executable t))'
$lisp --load "maxima-build.lisp" --eval "$final"

cp -v fremax2 ../../bin

 

