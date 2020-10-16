#!/bin/bash

#convert tiff to png

## An IF statement to provide an error statement and exit if not input file provided

if [ $# -eq 0 ]; then  
    echo "No input files specified on command line!  Error!!" >&2
    exit 1
fi

##Convert all tiff files to png and moving them to the data directory

for f in *.tif; 
    do  
        echo "Converting $f"; 
        convert "$f"  "$(basename "$f" .tif).png"; 
        mv $f.png ../data/ #move $f.png to the data directory
    done

