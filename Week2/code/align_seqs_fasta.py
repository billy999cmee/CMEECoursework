# Two example sequences to match

#with open('../data/test.fasta', 'r') as f:
    #seq1 = f.read()

#with open('../data/', 'r') as d:
    #seq2 = d.read()

## Import sys, for positional arguments usage
import sys
seq1 = sys.argv[1]
seq2 = sys.argv[2]

## Use Try instead of IF because it can be a lot less explicit, if line11/12 doesnt work then do this
try:
    seq1 = sys.argv[1]
    seq2 = sys.argv[2]
except IndexError:      #IndexError appears when line11/12 doesn't work
    print ("No input files, we are going to run random fasta files from the data directory!")
    with open('../data/407228326.fasta', 'r') as f:
        seq1 = f.read()
    with open('../data/407228412.fasta', 'r') as d:
        seq2 = d.read()

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


g = open('../sandbox/test1.txt','w')
L = [my_best_align, str(my_best_score)] #both must be string! My best score is int originally!
g.writelines(L)
g.close()