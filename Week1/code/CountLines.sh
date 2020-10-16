#!/bin/bash

## An IF statement to give and error statement and exit if no input file is provided
if [ $# -eq 0 ]; then  ## $# the number of positional arguments in the command
    echo "No input files specified on command line!  Error!" >&2
    exit 1
else
    echo "There is an input file given on command line,  it is $1 !"
fi

NumLines=`wc -l < $1`
echo "The file $1 has $NumLines lines"
echo
