#!/bin/bash


# If file is present, create a pdf file, open it and remove other unneccesary files, otherwise print error and exit if no file provided
if [ ! -z $1 ] ; then

    # Compile latex file and create a pdf file
        pdflatex $1.tex
        pdflatex $1.tex
        bibtex $1
        pdflatex $1.tex
        pdflatex $1.tex
        evince $1.pdf &

    # Cleanup !
    rm *.aux
    rm *.log
    rm *.bbl
    rm *.blg
else
echo "you need a file!"
exit 1
fi

