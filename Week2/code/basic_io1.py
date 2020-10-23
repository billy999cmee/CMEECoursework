#Open a file for reading
f = open('../sandbox/test.txt', 'r')
#use implicit for loop
# if the object is a file, python will cycle over lines
for line in f:
    print(line)

#close the file
f.close()

#same example, skip blank lines
f = open('../sandbox/test.txt', 'r')
for line in f:
    if len(line.strip()) > 0:
        print(line)

f.close()

#The for line in f is an implicit loop — 
# implicit because stating the range of things in f to loop over in this way allows python to handle any kind of objects to loop through.

#For example, if f was an array of numbers 1 to 10, it would loop through them

#Another example: if f is a file, as in the case of the script above, it will loop through the lines in the file.

#if len(line.strip()) > 0 checks if the line is empty. Try ? to see what .strip() does.

#There are indentations in the code that determine what is and is not in side the for and if statements. 
# If you get errors or unexpected outputs, it will very likely be because of wrong or missing indentations.