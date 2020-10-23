#!/usr/bin/env python3

"""Some functions exemplifying the use of control statements"""
#docstrings are considered part of the running code (normal comments are
#stripped). Hence, you can access your docstrings at run time.
__author__ = 'Billy Lam (yu.lam17@imperial.ac.uk)'
__version__ = '0.0.1'

import sys
import doctest #Import the doctest module

def even_or_odd(x=0): # if not specified, x should be 0

    """Find whether x is even or odd.
    
    >>> even_or_odd(10)
    '10 is Even!'
    
    >>> even_or_odd(5)
    '5 is Odd!'
    
    whenever a float is provided, then the closest integer is used:
    >>> even_or_odd(3.2)
    '3 is odd!'
    
    in case of negative numbers, the positive is taken:
    >>> even_or_odd(-2)
    '-2 is Even!'
    
    """
    #Definte function to be tested

    if x % 2 == 0:#The condition if
        return "%d is Even!" % x
    return "%d is Odd!" % x

def main(argv):
    print(even_or_odd(22))
    print(even_or_odd(33))
    return 0



if (__name__ == "__main__"):
    status = main(sys.argv)

doctest.testmod() # To run with embedded tests