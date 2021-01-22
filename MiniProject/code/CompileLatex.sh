#!/bin/bash

""" Compile Latex script to create a pdf """

__appname__ = 'CompileLatex.sh'
__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
__version__ = '0.0.1'

echo "Compiling report!"

## Compile
pdflatex MiniProject.tex
pdflatex MiniProject.tex
bibtex MiniProject
pdflatex MiniProject.tex
pdflatex MiniProject.tex

## Cleanups!
rm *~
rm *.aux
rm *.dvi
rm *.log
rm *.nav
rm *.out
rm *.snm
rm *.toc

## Move write up to results directory
mv MiniProject.pdf ../results/

## Display the pdf
evince ../results/MiniProject.pdf