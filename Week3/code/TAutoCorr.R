## Autocorrelation with weather

#__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
#__version__ = '3.6.3'

#Load ats
load('../data/KeyWestAnnualMeanTemperature.RData')

#Load package
library(tidyverse)

#new column of ats$Temp shifted by one 
df_after<-ats %>%
  mutate(second_col=Temp) %>% #copy column
  mutate_at(c("second_col"), list(lead), n = 1 )#shift by 1 with lead() 

#ats$temp1 <- ats$Temp[2:100, ], IT DOESNT WORK, ROW NUMBERS DIFF


#Calculate the correlation coefficient for t and t-1
# this is the observed correlation
obss <- cor(df_after$Temp, df_after$second_col, use = "complete.obs") 
# 0.3261697

# Randomly permuting time series by 10000 times
nreps <- 10000
random1 <- numeric(nreps)
for (i in 1:nreps){
  Y <- df_after$Temp
  X <- sample(df_after$second_col, 100, replace = F)
  random1[i] <- cor(X, Y, use = "complete.obs")
}

#Calculate p-value
p_val <- length(random1[random1 >= obss])/nreps

# Plot!
hist(random1, breaks = 50,
     main =  "Distribution of correlation coefficients",
     xlab = "Correlation coefficients")
obss <- round(obss, digits = 3)
legend(.22, 200, obss, bty = "n") #bty = n gets rid of the box
arrows(.28, 130, .326, 10)
