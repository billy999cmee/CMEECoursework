#!/usr/bin/env python3

""" Pipeline for the overall workflow of the miniproject """

__appname__ = 'run_MiniProject.py'
__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
__version__ = '0.0.1'

### Import module ###
import subprocess

### Workflow ###
# Data preparation
subprocess.Popen("Rscript Pop_prep.R", shell=True).wait() 
# Model fitting with predicted starting values only
subprocess.Popen("Rscript Pop_log_model.R", shell=True).wait() 
# Model fitting with sampling starting values 
subprocess.Popen("Rscript trial_log_fit.R", shell=True).wait() 
# Model analysis
subprocess.Popen("Rscript Pop_analysis.R", shell=True).wait() 
# Report writing and compile
subprocess.Popen("./CompileLatex.sh", shell=True).wait() 