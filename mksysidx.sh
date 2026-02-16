#!/bin/sh
rm -i system-index.txt
for a in */*.asd; do echo spadlib/$a >> system-index.txt ;done


# cat ../system-index.txt system-index.txt  | sort | uniq > ../system-index.txt
# or
#sort -u  ../system-index.txt system-index.txt  > ../system-index.txt
