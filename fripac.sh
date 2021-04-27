#!/bin/bash


function usage()
{
cat <<ENDUSAGE

fripac.sh v1.0.0 -- FriCAS Package Manager (using QuickLisp)

Usage: $0 [OPTION]... projectName

  -a | --author  c     Author    
  -l | --license c     License
  -v | --version c     Version
  -d | --descr   c     Short description
  
  --deps  c            Dependencies

  -h | --help  Help

Example: $ fripac.sh MyProject 
  will create a folder MyProject containing MyProject.asd for ASDF/QuickLisp
  and the following subdirectories: docs, src, lib and test. 

ENDUSAGE
}

[[ $# == 0 ]]       && { usage;exit; };
[[ $1 == -h ]]      && { usage;exit; };
[[ $1 == --help ]]  && { usage;exit; };


# ProjectName + asdf/lisp file
NAME=$BASH_ARGV
ASDFILE=$NAME.asd
LSPFILE=$NAME.lisp


# ----------------
# config (default)
# ----------------
AUTHOR="Your name"
VERSION="1.0.0"
DESCR="Short description"
LICENSE="MIT"
DEPS=""



# --------------
# option parsing
# --------------
while [[ $# -gt 1 ]]
do
arg="$1"

case $arg in
    -a|--author)
    AUTHOR="$2"
    shift
    ;;
    -v|--version)
    VERSION="$2"
    shift
    ;;
    -d|--descr)
    DESCR="$2"
    shift
    ;;
    -l|--license)
    LICENSE="$2"
    ;;
    -s|--srcdir)
    SRCDIR="$2"
    ;;
    --deps)
    DEPS="$2"
    ;;
    --comps)
    COMPS="$2"
    ;;
    *)
            # unknown option
    ;;
esac
shift # past argument or value
done


if [ ! -d  $NAME ]; then
  mkdir -v -p $NAME
fi

if [ -d  $NAME ]; then
  cd $NAME;
  mkdir -v -p ./docs ; touch ./docs/README ;
  mkdir -v -p ./src  ;
  mkdir -v -p ./lib  ; touch ./lib/README ;
  mkdir -v -p ./test ; touch ./test/README ;
else
  exit 1  
fi

## write ASDF file
echo "(in-package :common-lisp-user)" >> $ASDFILE
echo
echo "(asdf:defsystem #:$NAME" >> $ASDFILE
echo "  :serial t"  >> $ASDFILE
echo "  :description \"$DESCR\"" >> $ASDFILE
echo "  :version \"$VERSION\"" >> $ASDFILE
echo "  :author \"$AUTHOR\"" >> $ASDFILE
echo "  :license \"$LICENSE\"" >> $ASDFILE
echo "  :depends-on ($DEPS)" >> $ASDFILE
echo "  :pathname \"src/\"" >> $ASDFILE
echo "  :components ((:file \"$NAME\")))" >> $ASDFILE

echo $ASDFILE written.

## write LISP file
LSP=src/$LSPFILE

echo ";;;"  >> $LSP
echo ";;; ASDF/QuickLisp" >> $LSP
echo ";;;"  >> $LSP
echo "(defparameter *$NAME* (asdf:system-source-directory :$NAME))" >> $LSP
echo >> $LSP
echo "(defun |compile_$NAME| ()" >> $LSP
echo "  (progn" >> $LSP
echo "  (|doSystemCommand| (format nil \"cd ~Alib\" *$NAME*))" >> $LSP
echo "  (|doSystemCommand| (format nil \"compile ../src/$NAME.spad )quiet\"))))" >> $LSP
echo >> $LSP
echo "(defun |load_$NAME| ()" >> $LSP
echo "  (if (probe-file (format nil \"~Alib/$NAME.NRLIB/$NAME.lsp\" *$NAME*))" >> $LSP
echo "     (|doSystemCommand| (format nil \"lib )dir ~Alib/\" *$NAME*))" >> $LSP
echo "     (|compile_$NAME|)))" >> $LSP
echo >> $LSP
echo "(defun |test_$NAME| ()" >> $LSP 
echo "  (if (probe-file (format nil \"~Atest/test_$NAME.input\" *$NAME*))" >> $LSP
echo "    (|doSystemCommand| (format nil \"read ~Atest/test_$NAME )quiet\" *$NAME*))" >> $LSP
echo "    (print \"Test file not found ...\")))" >> $LSP
echo >> $LSP
echo "(catch 'spad_reader (|load_$NAME|))" >> $LSP
echo >> $LSP
  
echo $LSP written.
echo 
echo Package $NAME created.
echo Now you have to copy $NAME.spad to 'src'. 
