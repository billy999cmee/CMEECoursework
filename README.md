**Project name / title:**
<br> My CMEE Coursework Repository

**Brief description:**
<br> This is the MRes Computational Methodology in Ecology and Evolution (CMEE) at Imperial College London, it is a beginner's course in computer programming which includes several languages such as R, Python and Bash shell scripting. This project will include all the practical scripts of each chapter that are required for the completion of this course and will be updated on a weekly basis.

All of the scripts are based on:
<br> https://mhasoba.github.io/TheMulQuaBio/intro.html

All of the data are obtained from:
<br> https://github.com/mhasoba/TheMulQuaBio

**Languages:**
<br> GNU bash, version 5.0.17(1)-release (x86_64-pc-linux-gnu)
<br> Python3
<br> R 4.0.2

**Dependencies:**
<br> Week1:
<br> Imagemagick
<br> Latex

<br> Week2:
<br> csv package
<br> sys module
<br> doctest module

<br> Week3:
<br> tidyverse package
<br> reshape2 package
<br> ggplot2 package
<br> maps package
<br> rworldmap

**Installation:** 

<u> Week1  
<br> tiff2png.sh: apt install imagemagick
<br> FirstExample.tex: sudo apt-get install texlive-full texlive-fonts-recommended texlive-pictures texlive-latex-extra imagemagick  

<u> Week2
<br> basic_csv.py: type "Import csv" in your python script 
<br> sysargv.py, align_seqs.py: type "import sys" at the beginning of your python script 
<br> cfexercises1.py, test_control_flow.py: type "import doctest" at the beginning of your python script

<u> Week3
<br> TAutoCorr.R, DataWrangTidy.R: type "install.packages("tidyverse")" at the R terminal and type library(tidyverse) at the beginning of your R script to load it 
<br> DataWrang.R: type "install.packages("reshape2")" at the R terminal and type library(reshape2) at the beginning of your R script to load it 
<br> MyBars.R, Girko.R, PP_Regress.R, GPDD_Data.R, plotLin.R: type "install.packages("ggplot2")" at the R terminal and type library(ggplot2) at the beginning of your R script to load it 
<br> GPDD_Data.R: type "install.packages("maps")" at the R terminal and type library(maps) at the beginning of your R script to load it 
<br> GPDD_Data.R: type "install.packages("rworldmap")" at the R terminal and type library(rworldmap) at the beginning of your R script to load it 

**Project structure and Usage:**
<br> There is a sub-directory for each week, within each of those sub-directories there is a standard structure of code, data, results and sandbox. The code directory contains all the codes, the data directory contains all the data downloaded for the practicals (see brief description section to obtain the data from the link), results will contain all the results generated from practicals and sandbox is a directory filled with many test files which will not be assessed on and will be git ignored. Sometimes, a sub-directory called writeup will appear in some of the weekly directories whhich will consist of Latex written work, pdf and png files.

**Author name and contact**
<br> Billy Lam
<br> ykl17@ic.ac.uk