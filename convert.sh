#!/bin/bash

# go through every file in the directory
for f in `ls *.cel`; do
    PNM="$f.pnm"
    PNG="$f.png"
    TRANS=`~/dev/smooch/cel2pnm $f $1 $PNM`
    pnmtopng -transparent $TRANS $PNM > $PNG
    rm $PNM
done
