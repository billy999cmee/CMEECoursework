#__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
#__version__ = '3.6.3'

# Preallocation

# A for loop which R has to re-size the vector and re-allocate the memory

NoPreallocFun <- function(x){
  a <- vector() # empty vector
  for (i in 1:x) {
    a <- c(a, i)
    print(a)
    print(object.size(a))
  }
}

system.time(NoPreallocFun(10))

# A for loop which pre-allocates all the values in a vector, no memory 
# re-allocation needed

PreallocFun <- function(x){
  a <- rep(NA, x) # pre-allocated vector
  for (i in 1:x) {
    a[i] <- i
    print(a)
    print(object.size(a))
  }
}

system.time(PreallocFun(10))

#Vectorization, operations which apply to whole data str at one go, rather 
#than individual elements