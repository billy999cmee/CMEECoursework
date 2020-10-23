#!/usr/bin/env python3

"""Practical on DNA sequence alignment"""

__appname__ = 'align_seqs.py'
__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
__version__ = '0.0.1'

#Import
import sys

# Two example sequences to match

if len(sys.argv) == 1:
    with open(sys.argv[1], 'r') as f:
        seqs = f.read()
        seqs = seqs.split(",")
        seq1, seq2 = seqs[0], seqs[1]
else:
    seq2 = "ATCGCCGGATTACGGG"
    seq1 = "CAATTCGGAT"



#with open('../data/test1.csv', 'r') as f:
    #seqs = f.read()

#seqs = seqs.split(",")
#seq1, seq2 = seqs[0], seqs[1]


# Assign the longer sequence s1, and the shorter to s2
# l1 is length of the longest, l2 that of the shortest

l1 = len(seq1)
l2 = len(seq2)
if l1 >= l2:
    s1 = seq1
    s2 = seq2
else:
    s1 = seq2
    s2 = seq1
    l1, l2 = l2, l1 # swap the two lengths

# A function that computes a score by returning the number of matches starting
# from arbitrary startpoint (chosen by user)
def calculate_score(s1, s2, l1, l2, startpoint):
    """Giving scores on how well two DNA sequences align"""
    matched = "" # to hold string displaying alignements
    score = 0
    for i in range(l2):
        if (i + startpoint) < l1: #i is which loop which index
            if s1[i + startpoint] == s2[i]: # if the bases match
                matched = matched + "*"
                score = score + 1
            else:
                matched = matched + "-"

    # some formatted output
    print("." * startpoint + matched)           
    print("." * startpoint + s2)
    print(s1)
    print(score) 
    print(" ")

    return score

# Test the function with some example starting points:
# calculate_score(s1, s2, l1, l2, 0)
# calculate_score(s1, s2, l1, l2, 1)
# calculate_score(s1, s2, l1, l2, 5)

# now try to find the best match (highest score) for the two sequences
my_best_align = None
my_best_score = -1

for i in range(l1): # Note that you just take the last alignment with the highest score
    z = calculate_score(s1, s2, l1, l2, i)
    if z > my_best_score:
        my_best_align = "." * i + s2 # think about what this is doing!
        my_best_score = z 
print(my_best_align)
print(s1)
print("Best score:", my_best_score)


g = open('../results/aligned_seq_with_score.txt','w')
L = [my_best_align, '\n', str(my_best_score)] #Both must be string otherwise it cant't be written! My best score is int originally!
g.writelines(L)
g.close()