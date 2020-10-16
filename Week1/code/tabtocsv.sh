#!/bin/bash
# Author: Billy Lam yu.lam17@imperial.ac.uk
# Script: tabtocsv.sh
# Description: substitute the tabs in the files with commas
#
# Saves the output into a .csv file
# Arguments: 1 -> tab delimited file
# Date: Oct 2020

## An IF statement to give an error statement and exit if no input files are provided

if [ $# -eq 0 ]; then  ## $# the number of positional arguments in the command

    echo "No input files specified on command line.  Error." >&2
    exit 1
else
    echo "There is an input file given on command line.  It is $1 !"
fi

echo "Creating a comma delimited version of $1 ..."

cat $1 |tr -s "\t" "," >> $1.csv #tr -s replaces each sequence of a repeated character with a single occurence of that character, replace all tabs with a ,

echo "Moving $1.csv to data directory"

mv $1.csv ../data/

echo "Done!"
exit


