#!/bin/bash
redsrc=reduce-algebra-code
redsvn=https://svn.code.sf.net/p/reduce-algebra/code/trunk

# Get REDUCE trunk if necessary
if [ ! -d "$redsrc" ]; then
  svn checkout $redsvn $redsrc
fi 

# Go to folder 'common-lisp' & show svn ino
cd $redsrc/common-lisp
svn info

if [ -f fremax ]; then
   echo SBCL image fremax already exists in $redsrc/common-lisp. 
   echo Delete it in order to rebuild it ;
   exit 1
fi

# Phase I:  build redsbcl as advertised
# ./build.sh -l sbcl

( ./build.sh -l sbcl -c )


# Find line number of save!-reduce!-image in build.sh
srb=$(grep -n save\!-reduce build.sh | cut -d : -f 1)
echo "looking for save\!-reduce... function in build.sh"
echo "found at line number: " $srb

# Because we want a new image without :toplevel
# function, we have to replace the save-reduce-image
# function by SBCL: (save-lisp-and-die ....)
repstr='(sb-ext:save-lisp-and-die "fremax" :executable t)'

if [[ -z "$srb" ]]; then
  echo "function save-reduce-image not found in build.sh :-("
  echo "something went wrong ..."
  exit 1
fi

# Replace save-reduce by repstr ....
echo "replacing save-reduce function by: " $repstr
echo "in new script: build_fremax.sh"
# 
if [ ! -f build_fremax.sh ]; then
  sed  $srb's/.*/'"$repstr"'/' build.sh > build_fremax.sh
fi

if [ -f build_fremax.sh ]; then
  chmod a+x build_fremax.sh;
  ( ./build_fremax.sh -l sbcl )
fi

if [ -f fremax ]; then
   echo SBCL image $redsrc/common-lisp/fremax built. ;
   mkdir -p ../../bin
   cp -v fremax ../../bin/fremax1 
   exit 0 ; else
   echo *** failed. ;
   exit 1; 
fi








