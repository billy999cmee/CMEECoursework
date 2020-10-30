#__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
#__version__ = '3.6.3'

# "Catching" errors

#Sampling from a pop, only if at least 30 unique samples are obtained that we
#will takes its mean
doit <- function(x){
  temp_x <- try(sample(x, replace = TRUE), FALSE)
  if(length(unique(temp_x)) > 30) {#only take mean if sample was sufficient
    print(paste("Mean of this sample was:", as.character(mean(temp_x))))
  } 
  else {
    stop("Couldn't calculate mean: too few unique values!")
  }
}

#Generate a population
popn <- rnorm(50)
hist(popn)

#Use lapply, repeat sampling for 15 times
#lapply(1:15, function(i) doit(popn))

#Use lapply with try
result <- lapply(1:15, function(i) try(doit(popn), FALSE)) #FALSE subdue errors

#Errors stored in the object result
class(result)
result

#We can also store the results by using a forloop
result <- vector("list", 15) #Preallocate/Initialize
for(i in 1:15) {
  result[[i]] <- try(doit(popn), FALSE)
}
