#!/bin/bash

for a in ../spadlib/*/src/*.spad
  do echo $a; 
       b=$(basename $a .spad); 
       d=$(dirname $a);
       ./spad2doc.py < $a  > $d/../docs/$b.html; 
       ./spad2doc.py < $a  > ./docs/$b.html; 
  done

cd ./docs
source mkidx.sh

echo DONE
