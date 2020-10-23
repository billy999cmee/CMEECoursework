#!/usr/bin/env python3

"""Examples of conditional functions for fine-grained control"""

__appname__ = 'tuple.py'
__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
__version__ = '0.0.1'

#Imports
import sys
import doctest

#Function 1
def foo_1(x):
    """ A function which shows the square root of x
    
    Args: 
        x (int): The first input

    Returns:
        Str: The square root of input
    """
    if not isinstance(x, int):   #This needs to be first otherwise a string input wont return the error
        return "The input {} is not a number!".format(x)
    elif x < 0:
        return "You need to enter a positive integer!"
    else: #When condition is met, if not
        return "The square root of {} is {}".format(x, x ** 0.5) # ** means power of

#Function 2
def foo_2(x, y):
    """Compare two numbers and print the larger number.
        
    Args: 
        x (int): The first input
        y (int): The second input

    Returns:
        Str: The larger number
    """
    if not isinstance(x, int):
        return "The input {} is not a number!".format(x) 
    elif not isinstance(y, int):
        return "The input {} is not a number!".format(y)   
    elif x > y: #The conditional IF of which is larger
        return "{} is larger!".format(x) #x is larger!
    return "{} is larger!".format(y) #y is larger!

#Function 3
def foo_3(x, y, z):
    """Swapping argument positions, between x and y, y and z.
       Position swawps will occur if x>y, y>z.
               
    Args: 
        x (int): The first input
        y (int): The second input
        z (int): The third input

    Returns:
        Str: The inputs in swapped orders, between x and y, y and z.
       """
    if not isinstance(x, int):
        return "The input {} is not a number!".format(x) 
    elif not isinstance(y, int):
        return "The input {} is not a number!".format(y)
    elif not isinstance(z, int):
        return "The input {} is not a number!".format(z)      
    if x > y: #The conditional if
        tmp = y
        y = x
        x = tmp #swap positions between x and y
        print("{} and {} swapped positions".format(x, y))
    if y > z: #The conditional if
        tmp = z
        z = y
        y = tmp #swap positiions between y and z
        print("{} and {} swapped positions".format(y, z))
    return [x, y, z] 

#Function 4
def foo_4(x):
    """Find the factorial of x (x!)
       
    Args: 
        x (int): The first input

    Returns:
        Str: The factorial of x
    """
    if not isinstance(x, int):
        return "The input {} is not a number!".format(x)
    elif x<0:
        return "The input needs to be a positive integer!"
    else: 
        result = 1
        for i in range(1, x + 1):
            result *= i # result = result * i
        return "The factorial of {} is {}".format(x, result)

#Function 5
def foo_5(x):
    """A function which allows the return of factorial of x
           
    Args: 
        x (int): The first input

    Returns:
        Str: The input x and the factorial of x
    """
    if not isinstance(x, int):
        return "The input {} is not a number!".format(x)
    elif x<0:
        return "The input needs to be a positive integer!"
    ##The recursive function
    def foo_5a(x): 
        """A recursive functiion (a function which calls itself) that 
        calculates the factorial of x.
           
        Args: 
            x (int): The first input

        Returns:
            Str: The factorial of x
        """
        if x == 1:
            return 1
        return x * foo_5a(x - 1) #if x is not equals to 1, calculate its factorial 
    ##Back to function 5
    fac = foo_5a(x)
    return "The factorial of {} is {}!".format(x, fac)

#Function 6
def foo_6(x): 
    """A function which calculates the factorial of x in a different way
           
    Args: 
        x (int): The first input

    Returns:
        Str: The factorial of x
    """
    if not isinstance(x, int):
        return "The input {} is not a number!".format(x)
    elif x<0:
        return "The input needs to be a positive integer!"
    else:
        facto = 1
        y = x #so we can return the original x input
        while x >= 1: #While loop
            facto = facto * x
            x = x - 1
        return "The factorial of {} is {}".format(y, facto)

#Output evluations of all foo_x functions
def main(argv):
    print(foo_1(22))
    print(foo_1("A string"))
    print(foo_1(-2))
    print(foo_2(1, 2))
    print(foo_2(1, "A string"))
    print(foo_2("A string", 2))
    print(foo_3(3, 2, 1))
    print(foo_3(3, 2, "A string"))
    print(foo_3(1, 11, 2))
    print(foo_4(5))
    print(foo_4(-5))
    print(foo_4("A string"))
    print(foo_5(1))
    print(foo_5(-1))
    print(foo_5(5))
    print(foo_5("A string"))
    print(foo_6(2))
    print(foo_6(-2))
    print(foo_6("A string"))
    return 0

#Makes sure the functions are called from this main script
if (__name__ == "__main__"):
    status = main(sys.argv)
    sys.exit(status)