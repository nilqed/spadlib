#!/bin/bash

rm index.html
rm index

echo "<!DOCTYPE html>" >> index
echo "<html>" >> index
echo "<head>" >> index
echo "</head>" >> index
echo "<body>" >> index

echo "<h2 style='color:blue;'>Index</h2>" >> index;

for a in *.html; 
  do
    g=$(grep -F "<h2 style='color: blue;'>" $a); 
    h=$(echo $g | sed -e 's/h2/h4/g');
    k=$(echo $h | sed -e 's/blue/black/g');
    echo $k >> index;
    echo "==> <a href=./$a>$(basename $a .html)</a>" >> index;
    echo "<hr width="100%" size="1">" >> index;
  done; 

echo "</body>" >> index
echo "</html>" >> index

mv index index.html


