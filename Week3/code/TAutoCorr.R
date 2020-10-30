## Autocorrelation with weather

#__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
#__version__ = '3.6.3'

load('../data/KeyWestAnnualMeanTemperature.RData')

library(tidyverse)

#new column to ats
df_after<-ats %>%
  mutate(second_col=Temp) %>% #copy column
  mutate_at(c("second_col"), list(lead), n = 1 )#shift by 1 with lead() 

#ats$temp1 <- ats$Temp[2:100, ], IT DOESNT WORK, ROW NUMBERS DIFF

#Transpose and remove year
tats <- t(ats)
tats <- as.data.frame(tats)
tats <- tats[-c(1), ] #remove the first row

#new data frame
tp2 <- c(24.67, 24.72)
tp <- c(23.75, 24.67)
new <- data.frame(yr, tp)

#Calculate the correlation coefficient
cor(ats$Year, ats$Temp) #0.533

st <- c()
for (i in 1:length(ats$Year)){
  result <- cor(df_after$Temp[i, ], df_after$second_col[i, ], use = "complete.obs")
  print(result)
}

for (i in unique(ats$Year -1)) {
  hi <- cor(ats[i, ], ats[i+1, ])
  print(hi)
}
