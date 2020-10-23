#!/usr/bin/env python3
# Filename: using_name.py

if __name__== '__main__':
    print('This program is being run by itself')
else:
    print('I am being imported bby another module')

print("This module's name is: " + __name__)

## if run script, module name will be main
## if import, then it will be considered as side script adn wont be considered as main