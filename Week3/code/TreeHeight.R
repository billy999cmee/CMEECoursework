#__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
#__version__ = '3.6.3'

# This function calculates the heights of trees given distance of each tree 
# from its base and angle to its top, using  the trigonometric formula 
#
# height = distance * tan(radians)
#
# ARGUMENTS
# degrees:   The angle of elevation of tree
# distance:  The distance from base of tree (e.g., meters)
#
# OUTPUT
# The heights of the tree, same units as "distance"

# Load the data
tree <- read.csv('../data/trees.csv')

#Create a new file 
write.csv(tree, "../sandbox/TreeHts.csv", row.names = FALSE) #stop extra column

treehts <- read.csv('../sandbox/TreeHts.csv')

# Tree height calculation function
TreeHeight <- function(degrees, distance){
  radians <- degrees * pi / 180
  height <- distance * tan(radians)
  return (height)
}

#Create a new column
treehts$Tree.Height.m <- 0

#FOR loop to calculate the tree height for every row
for (i in 1:dim(tree)[1]){ #dim shows column and row, [1] only rows
  n <- TreeHeight(tree[i,3], tree[i,2]) # so 1:..[1] = start from row 1 and move
  treehts$Tree.Height.m[i] <- n # down by 1 row every time
}


#Export the data into the result directory
write.csv(treehts, "../results/TreeHts.csv")
