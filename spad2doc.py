#!/usr/bin/env python3

# spad2doc < file.spad extracts the doc strings starting with (++).
# and creates a HTML5 file.
# created: Fr 25 Okt 2024 16:14:37 CEST / kfp

import sys

#debug print(sys.argv)
lines = sys.stdin.readlines()

flag=True
intro=True
blocks = list()
nol = len(lines)

# Find blocks of packages, domains, categories
# They all should start with ")abbrev"
for k in range(len(lines)):
  if lines[k].startswith(")abbrev"):
    blocks.append(k)
else:
  blocks.append(k)
  #debug print(blocks)

print("<!DOCTYPE html>")
print("<html>")
print("<head>")
print("<title>SPADLIB (spad2doc)</title>")
print("</head>")
print("<body>")

for j in range(len(blocks)-1):
  headlst = lines[blocks[j]].split()
  #debug print(headlst)
  print("<h2 style='color: blue;'>"+headlst[3]+"</h2>")
  for i in range(blocks[j],blocks[j+1]):
    if lines[i].startswith("++"):
      #debug: print("INTRO:"+str(i))
      if intro:
        print("<pre style='color: brown;'>")
        intro=False
      print(lines[i][2:].rstrip())
    else:
      if not intro:
        print("</pre>")
        intro=True
      b=lines[i].lstrip()
      if b.startswith("++"):
        if flag:
          print("<strong><code>"+lines[i-1].strip()+"</code></strong>")
          flag = False
          print("<pre style='color: darkgreen;'>")
        #debug print("DECL:"+str(i))
        print(lines[i].strip()[2:])
      else:
        flag = True
        print("</pre>")
    

print("</body>")
print("</html>") 

