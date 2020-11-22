#!/usr/bin/env python3

"""Profiling1"""

__appname__ = 'profileme.py'
__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
__version__ = '0.0.1'


def my_squares(iters):
    """ Calculates the square of the input numbers and displaying the results """
    out = []
    for i in range(iters):
        out.append(i ** 2)
    return out

def my_join(iters, string):
    """ Appending the string input next to the numeric input, separated by a comma """
    out = ''
    for i in range(iters):
        out += string.join(", ")
    return out

def run_my_funcs(x,y):
    """ Joining two functions together (my_join and my_squares) and displaying their outputs in a single line """
    print(x,y)
    my_squares(x)
    my_join(x,y)
    return 0

run_my_funcs(10000000,"My string")