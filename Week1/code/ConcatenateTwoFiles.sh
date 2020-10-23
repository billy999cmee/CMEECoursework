#!/bin/bash
## concatenate two files into a new file

## An IF statement to provide an error statement and exit in the absence of 2 input files and a provided filename for $3 

if [ $# -eq 0 ] || [ $# -eq 1 ] || [ $# -eq 2 ]; then  ## $# is the number of positional arguments in the command,
    echo "You need two input files and the name of an output file on the command line!  Error!!" >&2
    exit 1
else
    echo "There are two input files and the name of an output file given on the command line!  It is $1, $2 and $3 as the name of the output file!"
    cat $1 > $3 #concatenate and print $1, redirect to $3
    cat $2 >> $3 #concatenate and print $2, redirect to $3
    echo "Merged File is"
    cat $3
    echo "Moving $3 to data"
    mv $3 ../data/ # move $3 to the data directory
fi


