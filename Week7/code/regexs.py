#!/usr/bin/env python3

"""Regex examples"""

__appname__ = 'regexs.py'
__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
__version__ = '0.0.1'

# Package
import re

# Examples
my_string = "a given string"
match = re.search(r'\s', my_string)
print(match) # shows a match was found
# To view the match use:
match.group()

# Another pattern
match = re.search(r'\d', my_string)
print(match)

# Know whether a pattern is matched
MyStr = 'an example'
match = re.search(r'\w*\s', MyStr) # what pattern is this?
if match:                      
    print('found a match:', match.group()) 
else:
    print('did not find a match')    

# Some other examples
match = re.search(r'2' , "it takes 2 to tango")
match.group()

match = re.search(r'\d' , "it takes 2 to tango")
match.group()

match = re.search(r'\d.*' , "it takes 2 to tango")
match.group()

match = re.search(r'\s\w{1,3}\s', 'once upon a time')
match.group()

match = re.search(r'\s\w*$', 'once upon a time')
match.group()

# Same but directly returning the matched groups
re.search(r'\w*\s\d.*\d', 'take 2 grams of H2O').group()

re.search(r'^\w*.*\s', 'once upon a time').group()

re.search(r'^\w*.*?\s', 'once upon a time').group() # used ? to make matching non-greedy, only the first one matched

re.search(r'<.+>', 'This is a <EM>first</EM> test').group() #greedy, only want <EM>

re.search(r'<.+?>', 'This is a <EM>first</EM> test').group() # make + lazy

re.search(r'\d*\.?\d*','1432.75+60.22i').group() # return the first 123123.2323

re.search(r'[AGTC]+', 'the sequence ATTCGT').group()

re.search(r'\s+[A-Z]\w+\s*\w+', "The bird-shit frog's name is Theloderma asper.").group()

MyStr = 'Samraat Pawar, s.pawar@imperial.ac.uk, Systems biology and ecological theory'
match = re.search(r"[\w\s]+,\s[\w\.@]+,\s[\w\s]+",MyStr)
match.group()

MyStr = 'Samraat Pawar, s-pawar@imperial.ac.uk, Systems biology and ecological theory'
match = re.search(r"[\w\s]+,\s[\w\.@]+,\s[\w\s]+",MyStr)
match.group() #doesn't work for other email patterns

match = re.search(r"[\w\s]+,\s[\w\.-]+@[\w\.-]+,\s[\w\s]+",MyStr)
match.group()

