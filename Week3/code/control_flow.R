## Control flow tools

#__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
#__version__ = '3.6.3'

# IF statements

a <- TRUE
if (a == TRUE){
  print ("a is TRUE")
} else {
  print ("a is FALSE")
}

#Writing an IF statement in 1 line, lower code readability tho
z <- runif(1) ## Generate a uniformly distributed random number
if (z <= 0.5) {print ("Less than a half")}

# FOR loops

for (i in 1:10){
  j <- i * i
  print(paste(i, " squared is", j ))
}

for(species in c('Heliodoxa rubinoides', 
                 'Boissonneaua jardini', 
                 'Sula nebouxii')){
  print(paste('The species is', species))
}

v1 <- c("a","bc","def")
for (i in v1){
  print(i)
}

# WHILE loops, perform an operation till some condition is met
i <- 0
while (i < 10){
  i <- i+1
  print(i^2)
}