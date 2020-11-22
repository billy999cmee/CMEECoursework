#!/usr/bin/env python3

"""Comparing the run speed between LV1.py and LV2.py"""

__appname__ = 'run_LV.py'
__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
__version__ = '0.0.1'

# Import
import cProfile # gives total time and separate function times


# Able to read because dCR_dt was put inside another function
import LV1
cProfile.run('LV1.wrapping_dcrdt()', sort="tottime")

import LV2
cProfile.run('LV2.wrapping_dcrdt()', sort="tottime")