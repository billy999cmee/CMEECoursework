#__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
#__version__ = '3.6.3'

#### Preparation script of the population dataset ####

## Load package
library(dplyr)

## Load data
data <- read.csv("../data/LogisticGrowthData.csv", stringsAsFactors = F)

## Data inspection
unique(data$PopBio_units) # 4 unique response variable units
unique(data$Time_units) # 1 independent unit

# Make a new citation column by renaming one of the citations as it cause probs in ggtitle()
data$Citation2 <- gsub("Bernhardt, J.R., Sunday, J.M. and O’Connor, M.I., 2018. Metabolic theory and the temperature-size rule explain the temperature dependence of population carrying capacity. The American naturalist, 192\\(6\\), pp.687\\-697",
                       "Bernhardt JR, Sunday JM and OConnor MI, 2018. Metabolic theory", data$Citation) 

# Modifying species name for tidiness and potential errors in ggtitle()
data$Species <- gsub("[.]", " ", data$Species) 
data$Species <- gsub("spp |sp ", "species", data$Species) 
data$Species <- gsub(" 1| 2|  RDA R | 77| 62| 88| subspecies Carotovorum Pcc2| Strain 97", "", data$Species) 
data$Species <- gsub(" StrainCYA28", "", data$Species)


# Remove "-" in Medium
data$Medium <- gsub("-", " ", data$Medium)

# Checking for negative values and removing/converting them
#which(data$Time < 0) # use sum for total, 68
#which(data$PopBio < 0) # 23

#############################
data <- data[data$PopBio > 0, ] # remove negative pop values, if bact dies and 
data$Time <- abs(data$Time) # convert negatives in Time, 
#############################

# Make a new id column, id2 = clean unique column
data$id <- paste(data$Species, data$Temp, data$Medium, data$Citation)
data$id2 <- cumsum(!duplicated(data$id))

# Convert OD_595 to percentages by multiplying all 0D595 by 100
# nrow(OD2[OD2$PopBio<0,])
data$PopChange <- data$PopBio # make a copy of PopBio 
data$PopChange[which(data$PopBio_units=="OD_595")]<-data$PopChange[which(data$PopBio_units == "OD_595")]*100 # Change to optical density reading in percentages!

# Make a log PopBio and PopChange
data$logpop1 <- log(data$PopBio + 1)
data$logpop <- log(data$PopBio)
data$logc <- log(data$PopChange)

# Remove negative log values in logc due to 0.XX PopBio, otherwise NaNs will be produced
data <- data[data$logc > 0, ]

# logpop consists of tons of negative numbers, make a shift
#data$logshift <- data$logpop + abs(min(data$logpop))

# check any NAs/inf values in data frame
# apply(data, 2, function(x) any(is.na(x) | is.infinite(x))) 

# 
# #Remove NAs and inf from log pop column
# sum(is.infinite(data$logpop))
# data2 <- na.omit(data)
# df <- data2 %>% filter_all(all_vars(!is.infinite(.)))

# Export dataframe to the date directory
write.csv(data, "../data/Pop_clean.csv", row.names = F)

