#!/usr/bin/env python3

""" A template for writing Python programs
"""

__appname__ = 'boilerplate.py'
__author__ = 'Billy Lam ykl17@ic.ac.uk'
__version__ = '0.0.1'
__license__ = "License for this code/program"

## imports ##
import sys # module to interface our program with the operating system


## constants ##


## functions ##
def main(argv):
    """ Main entry point of the program """
    print('This is a boilerplate') # NOTE: indented using two tabs or 4 spaces
    return 0


if __name__ == "__main__": 
    """Makes sure the "main" function is called from command line, serves as the main script""" 
    ### make sure we are running hte main script, not the sub-script
    status = main(sys.argv)
    sys.exit(status)

