birds = ( ('Passerculus sandwichensis','Savannah sparrow',18.7),
          ('Delichon urbica','House martin',19),
          ('Junco phaeonotus','Yellow-eyed junco',19.5),
          ('Junco hyemalis','Dark-eyed junco',19.6),
          ('Tachycineata bicolor','Tree swallow',20.2),
         )

#(1) Write three separate list comprehensions that create three different
# lists containing the latin names, common names and mean body masses for
# each species in birds, respectively. 

Latin_names = [latin[0] for latin in birds] 

Common_names = [common[1] for common in birds]

Body_masses = [body[2] for body in birds]

# (2) Now do the same using conventional loops (you can choose to do this 
# before 1 !). 

## Latin names provided using for loop
latin = []
for latins in birds: #for every tupple in a list
    latin.append(latins[0]) #append the first element to it
print(latin)

##Common names provided using for loop
common = []
for commons in birds:
    common.append(commons[1])
print(common)

##Body mass provided using for loop
mass = []
for body in birds:
    mass.append(body[2])
print(mass)