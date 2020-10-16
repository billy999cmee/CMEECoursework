#!/bin/bash

## IF statement to exit if there isn't an input file
if [ $# -eq 0 ]; then  
    echo "No input files specified on command line!  Error!" >&2
    exit 1
fi

echo "Creating a space separated values file of $1 ..."

## Hard coding to convert .csv to .ssv, only works if your $1 is ../XX/XX.csv! It will work for all the csv files in the data directory cp from Temperatures

filename=$(echo $1 | rev | cut -f 1 -d '/' | rev | cut -f 1 -d '.') #cut filename so only filename exists, e.g. ../sandbox/test.txt to test

directory=$(echo $1 | cut -f 2 -d '/') #cut till only directory exists, e.g. ../sandbox/test.txt to sandbox

whole="../$directory/$filename" #have the whole directory out

cat $1 | tr -s "," " " >> $whole.ssv

echo "Done!"
exit