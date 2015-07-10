#!/bin/bash

# go through every file in the directory
for f in `ls *.cel`; do
    BASE=`basename $f .cel`
    PNM="$BASE.pnm"
    PNG="$BASE.png"
    echo "$1 $2"
    TRANS=`~/dev/smooch/cel2pnm -t $1`
    ~/dev/smooch/cel2pnm $1 $f $PNM
    pnmtopng -transparent $TRANS $PNM > $PNG
    rm $PNM
done
