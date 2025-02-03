#!/bin/bash
frisrc="fricas"
frigit="https://github.com/fricas/fricas.git"

# Get REDUCE trunk if necessary
if [ ! -d "$frisrc" ]; then
  git clone $frigit 
fi

sudo apt install libx11-dev libxt-dev libice-dev \
                    libsm-dev libxau-dev libxdmcp-dev libxpm-dev \
                      xvfb libgmp-dev texlive auctex dvipng 
                      
                      
mkdir -p fremax_build
cd fremax_build

where="/home/kfp/devel/fricas-fremax"
lisp="/home/kfp/devel/fremax/bin/fremax1 --dynamic-space-size 4096"
( ../fricas/configure  -with-lisp="$lisp" --prefix="$where" --enable-gmp )


#( make )
#( make install )
#( make check )


 
                  
