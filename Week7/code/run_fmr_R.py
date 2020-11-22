#!/usr/bin/env python3

"""Runs fmr.R from ipython3 terminal using subprocess"""

__appname__ = 'run_fmr_R.py'
__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
__version__ = '0.0.1'

import subprocess

## Runs fmr.R and checks whether the run was successful or not
subprocess.Popen("Rscript fmr.R 2> ../results/fmr_err.Rout", shell=True).wait()
with open('../results/fmr_err.Rout', 'r') as f: text = f.read()
if len(text) > 0: print("fmr.R failed to run!")
else: print("fmr.R ran successfully!")