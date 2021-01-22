#__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
#__version__ = '3.6.3'

#### Preparation script of the population dataset ####
cat("Starting data preparation")

## Load package
library(dplyr)

## Load data
data <- read.csv("../data/LogisticGrowthData.csv", stringsAsFactors = F)

## Data inspection
unique(data$PopBio_units) # 4 unique response variable units
unique(data$Time_units) # 1 independent unit

# Make a new citation column by renaming one of the citations as it cause problems in ggtitle()
data$Citation2 <- gsub("Bernhardt, J.R., Sunday, J.M. and O’Connor, M.I., 2018. Metabolic theory and the temperature-size rule explain the temperature dependence of population carrying capacity. The American naturalist, 192\\(6\\), pp.687\\-697",
                       "Bernhardt JR, Sunday JM and OConnor MI, 2018. Metabolic theory", data$Citation) 

# Modifying species name for tidiness and potential errors in ggtitle()
data$Species <- gsub("[.]", " ", data$Species) 
data$Species <- gsub("spp |sp ", "species", data$Species) 
data$Species <- gsub(" 1| 2|  RDA R | 77| 62| 88| subspecies Carotovorum Pcc2| Strain 97", "", data$Species) 
data$Species <- gsub(" StrainCYA28", "", data$Species)


# Remove "-" in Medium
data$Medium <- gsub("-", " ", data$Medium)


#############################
data <- data[data$PopBio > 0, ] # Remove negative pop values 
data$Time <- abs(data$Time) # Convert negatives in Time, 
#############################

# Make a new id column, id2 = clean unique numbering column
data$id <- paste(data$Species, data$Temp, data$Medium, data$Citation)
data$id2 <- cumsum(!duplicated(data$id))

# Convert OD_595 to percentages by multiplying all 0D595 by 100
# nrow(OD2[OD2$PopBio<0,])
data$PopChange <- data$PopBio # make a copy of PopBio 
data$PopChange[which(data$PopBio_units=="OD_595")]<-data$PopChange[which(data$PopBio_units == "OD_595")]*100 # Change to optical density reading in percentages!

# Make a log PopBio and PopChange
data$logpop1 <- log(data$PopBio + 1)

# Export dataframe to the date directory
write.csv(data, "../data/Pop_clean.csv", row.names = F)