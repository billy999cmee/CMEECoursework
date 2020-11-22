#!/usr/bin/env python3

"""Using subprocess and os"""

__appname__ = 'TestR.py'
__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
__version__ = '0.0.1'

""" This is blah blah"""

# Import modules
import subprocess, os

# Use the subprocess.os module to get a list of files and directories 
# in your ubuntu home directory 

os.listdir(os.environ['HOME'])

# Hint: look in subprocess.os and/or subprocess.os.path and/or 
# subprocess.os.walk for helpful functions



#################################
#~Get a list of files and 
#~directories in your home/ that start with an uppercase 'C'

# Type your code here:



# Get the user's home directory.
home = subprocess.os.path.expanduser("~")

FilesDirsStartingWithC = [filename for filename in os.listdir(home) if filename.startswith("C")]
print(FilesDirsStartingWithC)

# Create a list to store the results.
#FilesDirsStartingWithC = []

# Use a for loop to walk through the home directory.
#for (dir, subdir, files) in subprocess.os.walk(home):
    #if dir[0].startswith("C"):
        #print(dir)
    #elif subdir[0].startswith("C"):
        #print(subdir)
    #elif files[0].startswith("C"):
        #print(files)
    #FilesDirsStartingWithC.append(dir, subdir, files)

  
#################################
# Get files and directories in your home/ that start with either an 
# upper or lower case 'C'

# Type your code here:

FilesDirsStartingWithCc = [filename for filename in os.listdir(home) if filename.startswith(("C", "c"))]
print(FilesDirsStartingWithCc)

#################################
# Get only directories in your home/ that start with either an upper or 
#~lower case 'C' 

# Type your code here:

#p = os.listdir(os.environ['HOME'])

DirsStartingWithCc = [filename for filename in os.listdir(home) if filename.startswith(("C", "c")) & os.path.isdir('/home/billy/{}'.format(filename))]
print(DirsStartingWithCc)