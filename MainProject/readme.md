**Project name / title:**
<br> Driver effects of survival and spatiotemporal activity of a woodland rodent

**Brief description:**
<br> This thesis is submitted for the partial fulfillment of the requirements for the degree of MRes in Computational Methods in Ecology and Evolution at Imperial Colleg London. It aims to infer the drivers of survival and spatiotemporal activity of wood mouse using 3 years of trapping and location data collected at Nash's Copse, Silwood Park campus. Data analysis is divided into two sections and involves both R and program MARK. 

Data wrangling scripts include GISdataPrep.R, Wrangle.R and Wrangle2.R
<br> Survival analysis scripts include Survival.R and Survivalplots.R
<br> Activity analysis scripts include Spatiotemporal2Male.R, Spatiotemporal2Female.R and Spatiotemporal.R

**General workflow of R scripts:** 
<br> GISdataPrep -> Wrangle -> Wrangle2 -> Survival -> Survivalplots -> Spatiotemporal2Male -> Spatiotemporal2Female -> Spatiotemporal

**Description of R scripts:** 
<br> GISdataPrep.R reads all shapefiles, creates a map of the study site and matches trapping and data logger detection coordinates to particular habitat features
<br> Wrangle.R converts the data logger detection dataset to comprise of capture history, e.g. 100101101, and individual covariates
<br> Wrangle2.R performs data wrangling and transform both trapping and data logger datasets to fit GLMMs
<br> Survival.R performs survival analysis through accessing program MARK
<br> Survivalplots.R plots all predicted survival graphs from selected models
<br> Spatiotemporal2Male.R fis the male dataset to 4 GLMMs 
<br> Spatiotemporal2Female.R fis the female dataset to 4 GLMMs 
<br> Spatiotemporal.R consists of more GLMM analysis (extra, not necessarily needed), correlation test and plots all 8 GLMM results


**Language:**
<br> R (4.1.0)

**Software:**
<br> Program MARK
<br> Download instructions: http://www.phidot.org/software/mark/downloads/index.html

**Dependencies:**
<br> R:
<br> rgdal package
<br> ggplot2 package
<br> dplyr package
<br> reshape package
<br> tidyr package
<br> raster package
<br> RMark package
<br> grid package
<br> lme4 package
<br> lmerTest package
<br> performance package
<br> glmmTMB package
<br> MuMIn package
<br> DHARMa package
<br> ggpubr package


**Installation:** 
<br> GISdataPrep.R: type "install.packages("rgdal")" at your R terminal if you haven't yet installed it and type "library(rgdal)" at the beginning of your R script
<br> GISdataPrep.R, Wrangle.R, Wrangle2.R: type "install.packages("dplyr")" at your R terminal if you haven't yet installed it and type "library(dplyr)" at the beginning of your R script
<br> GISdataPrep.R, Survival.R, Survivalplots.R, Spatiotemporal.R: type "install.packages("ggplot2")" at your R terminal if you haven't yet installed it and type "library(ggplot2)" at the beginning of your R script
<br> Wrangle.R, Wrangle2.R: type "install.packages("tidyr")" at your R terminal if you haven't yet installed it and type "library(tidyr)" at the beginning of your R script
<br> Wrangle.R: type "install.packages("reshape")" at your R terminal if you haven't yet installed it and type "library(reshape)" at the beginning of your R script
<br> Wrangle2.R: type "install.packages("raster")" at your R terminal if you haven't yet installed it and type "library(raster)" at the beginning of your R script
<br> Survival.R: type "install.packages("RMark")" at your R terminal if you haven't yet installed it and type "library(RMark)" at the beginning of your R script
<br> Survivalplots.R: type "install.packages("grid")" at your R terminal if you haven't yet installed it and type "library(grid)" at the beginning of your R script
<br> Spatiotemporal2Male.R, Spatiotemporal2Female.R: type "install.packages("lme4")" at your R terminal if you haven't yet installed it and type "library(lme4)" at the beginning of your R script
<br> Spatiotemporal2Male.R, Spatiotemporal2Female.R: type "install.packages("lmerTest")" at your R terminal if you haven't yet installed it and type "library(lmertest)" at the beginning of your R script
<br> Spatiotemporal2Male.R, Spatiotemporal2Female.R: type "install.packages("performance")" at your R terminal if you haven't yet installed it and type "library(performance)" at the beginning of your R script
<br> Spatiotemporal2Male.R, Spatiotemporal2Female.R: type "install.packages("glmmTMB")" at your R terminal if you haven't yet installed it and type "library(glmmTMB)" at the beginning of your R script
<br> Spatiotemporal2Male.R, Spatiotemporal2Female.R: type "install.packages("MuMIn")" at your R terminal if you haven't yet installed it and type "library(MuMIn)" at the beginning of your R script
<br> Spatiotemporal2Male.R, Spatiotemporal2Female.R: download package from: https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html and type "library(DHARMa)" at the beginning of your R script
<br> Spatiotemporal.R: type "install.packages("ggpubr")" at your R terminal if you haven't yet installed it and type "library(ggpubr)" at the beginning of your R script

**Project structure and Usage:**
<br> Within this Mainproject directory, there are sub-directories including code, data, result, writeup (includes the pdf version of the thesis) and sandbox (this is used for testing and is gitignored)

**Author name and contact**
<br> Billy Lam
<br> ykl17@ic.ac.uk