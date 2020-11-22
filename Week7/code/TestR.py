#!/usr/bin/env python3

"""Testing python subprocesses by running TestR.R"""

__appname__ = 'TestR.py'
__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
__version__ = '0.0.1'

# Import packages
import subprocess


subprocess.Popen("Rscript --verbose TestR.R > ../results/TestR.Rout 2> ../results/TestR_errFile.Rout", shell=True).wait()
