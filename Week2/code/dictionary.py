#!/usr/bin/env python3

"""Dictionary practical"""

__appname__ = 'dictionary.py'
__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
__version__ = '0.0.1'

## Tupple in a list
taxa = [ ('Myotis lucifugus','Chiroptera'),
         ('Gerbillus henleyi','Rodentia',),
         ('Peromyscus crinitus', 'Rodentia'),
         ('Mus domesticus', 'Rodentia'),
         ('Cleithrionomys rutilus', 'Rodentia'),
         ('Microgale dobsoni', 'Afrosoricida'),
         ('Microgale talazaci', 'Afrosoricida'),
         ('Lyacon pictus', 'Carnivora'),
         ('Arctocephalus gazella', 'Carnivora'),
         ('Canis lupus', 'Carnivora'),
        ]

# Write a short python script to populate a dictionary called taxa_dic 
# derived from  taxa so that it maps order names to sets of taxa. 
# E.g. 'Chiroptera' : set(['Myotis lucifugus']) etc. 

taxa_dic = {}
for name,taxon in taxa:
        taxa_dic.setdefault(taxon, []).append(name) #dictionary method of setdefault convert the first parameter to key and second to the value of dict
print(taxa_dic)