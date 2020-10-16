#!/bin/bash

## Shows the use of variables ##

re='^[0-9]+$' #regex: + means check all inputs! (min of one to infinite times) and $ means end of line

MyVar='2'
echo 'the current value of the variable is' "$MyVar" 
echo '3'
read MyVar

## IF statement to make sure the input can't be empty

# Check if string is empty using -z (True if string is empty)   
if [ -z "$MyVar" ]; then
   printf '%s\n' "No input entered"    #%s means take the next arguement and print it as a string
   exit 1
fi

## I think an integer will make more sense but putting anything else for MyVar won't stop the script from working!

if ! [[ $MyVar =~ $re ]]; then
    printf '%s\n' "I think an integer will make more sense but its aight!"
fi

echo 'the current value of the variable is' "$MyVar"

## Reading multiple values ##
echo '2 3'
read a b

## Two IF statements to make sure the inputs can't be empty and must be an integer
 
if [ -z "$a" ] || [ -z "$b" ]; then # || means OR, && means AND
   printf '%s\n' "No input entered or not enough inputs entered"
   exit 1
fi

#check if string is an integer using -n (true if string is not empty)
if ! [[ $a =~ $re ]] || ! [[ $b =~ $re ]]; then
    printf '%s\n' "You need to enter numbers!"
    exit 1
fi

echo 'you entered' $a 'and' $b '. Their sum is:'
mysum=`expr $a + $b`
echo $mysum



#test.txt, cut -f 1 -d ".", result = test, cut the first .
