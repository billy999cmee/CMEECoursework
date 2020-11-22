#!/usr/bin/env python3

"""Using regex to print out specific info"""

__appname__ = 'blackbirds.py'
__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
__version__ = '0.0.1'

# Import packages
import re

# Read the file (using a different, more python 3 way, just for fun!)
with open('../data/blackbirds.txt', 'r') as f:
    text = f.read()

# replace \t's and \n's with a spaces:
text = text.replace('\t',' ')
text = text.replace('\n',' ')
# You may want to make other changes to the text. 

# In particular, note that there are "strange characters" (these are accents and
# non-ascii symbols) because we don't care for them, first transform to ASCII:

text = text.encode('ascii', 'ignore') # first encode into ascii bytes
text = text.decode('ascii', 'ignore') # Now decode back to string

# Now extend this script so that it captures the Kingdom, Phylum and Species
# name for each species and prints it out to screen neatly.

kingd = re.findall(r'Kingdom\s\w+', text)
kingd2 = [i.split(" ")[1] for i in kingd] #split strings

phylum = re.findall(r'Phylum\s\w+', text)
phylum2 = [i.split(" ")[1] for i in phylum]

species = re.findall(r'Species\s\w+\s\w+', text)
species2 = [i.split(" ", 1)[1] for i in species] #split(by space bar, split once)[stay for the second element]

def jn(x, y, z):
    """
    Joins the kingdom, phylum and species name together for each species
    
    Input parameters:
        x(list) = List of kingdom of 4 species
        y(list) = List of phylum of 4 species
        z(list) = List of the binomial name of 4 species
    """
    first = [x[0], y[0], z[0]]
    print(first)
    second = [x[1], y[1], z[1]]
    print(second)
    third = [x[2], y[2], z[2]]
    print(third)
    fourth = [x[3], y[3], z[3]]
    print(fourth)

jn(kingd2, phylum2, species2)

# Hint: you may want to use re.findall(my_reg, text)... Keep in mind that there
# are multiple ways to skin this cat! Your solution could involve multiple
# regular expression calls (slightly easier!), or a single one (slightly harder!)