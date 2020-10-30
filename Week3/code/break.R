## Breaking out of loop!

#Stop the loop when some condition is met, you might not want to loop forever!

#__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
#__version__ = '3.6.3'

i <- 0 #Initialize i
while(i < Inf) {
  if (i == 10) {
    break 
  } # Break out of the while loop! 
  else { 
    cat("i equals " , i , " \n")
    i <- i + 1 # Update i
  }
}

