#!/usr/bin/env python3

"""Tuple practical"""

__appname__ = 'tuple.py'
__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
__version__ = '0.0.1'

## Tupple in a tupple
birds = ( ('Passerculus sandwichensis','Savannah sparrow',18.7),
          ('Delichon urbica','House martin',19),
          ('Junco phaeonotus','Yellow-eyed junco',19.5),
          ('Junco hyemalis','Dark-eyed junco',19.6),
          ('Tachycineata bicolor','Tree swallow',20.2),
        )

# Birds is a tuple of tuples of length three: latin name, common name, mass.
# write a (short) script to print these on a separate line or output block by species 
# Hints: use the "print" command! You can use list comprehensions!

## for each tupple, print it out, each element is separated by ,
for a, b, c in birds:
    print(a, b, c, sep = ', ')