################ Data Wrangling for ################
############# Spatialtemporal activity #############

# Load libraries
library(dplyr)
library(raster)
library(tidyr)

# Read data
indi <- read.csv("../Data/capturehistoryCOMB4.csv", 
                 colClasses = c(ch = "character", sex = "factor", moonbin = "factor"))

space <- read.csv("../Data/gishabitats.csv")
moon <- read.csv("../Data/LunarIllumination.csv")

################ Space data frame data wrangling ################

# Subset space to only include Apodemus Sylvaticus
space <- subset(space, Species == "AS")

# Change space date column format
space$date.merge <- gsub("_", "/", space$date.merge)
space$date.merge <- as.Date(space$date.merge, format = "%d/%m/%y")
space$full.date <- as.Date(space$full.date, format = "%d/%m/%y")

# Remove NAs in the minute column
# Very low proportion of NAs to whole ind data frame!
space <- space[!is.na(space$minute),]

# Correct typo in dataset, f instead of F in sex column!
space$Sex <- gsub("f", "F", space$Sex)

############# New dataframe with each individual's total logger hits #############

# Might not be needed!

totalhits <- space %>%                                        
  group_by(ID) %>%                         
  summarise_at(vars(visit),              
               list(name = sum)) 

# Match logger hits to indi by ID
indi$totalhits <- totalhits$name[match(indi$ID, totalhits$ID)]

################ Create a new dataframe based on per night per ID ################
########################## For spatiotemporal activity ###########################


### General workflow :

# Sort row numbers, remove same data logger hits within 15 mins, create distance column in space, 
# then time column,
# add both columns to a new dataframe, distance/activity time = spatiotemporal activity!


# Combine time and date into one column
space1 <- space %>%
  unite(time, hour, minute, sec, sep = ":") 

# Duplicate time and date column for merging with dates later
space1$time2 <- space1$time
space1$date.merge2 <- space1$date.merge

# Change to date+time class
space1$time <- as.POSIXct(space1$time, format = "%H:%M:%S")

# Subtract 7 hours for the time column
# as A.sylvaticus are nocturnal
space1$time <- space1$time - 7*60*60

# Equalise all dates for the time column
space1$time <- as.POSIXct(sub("\\S+", "2021-07-19", space1$time))

# Sort row numbers by date, ID and time
space1 <- space1 %>%
  arrange(date.merge, ID, time)

# Separate time column, remove the incorrect dates
space1 <- tidyr::separate(space1, time, c("date", "time"), sep = " ")

# Merge the correct dates to the time column
space2 <- space1 %>%
  unite(datetime, date.merge2, time, sep = " ")

# Change to class date+time
space2$datetime <- as.POSIXct(space2$datetime, format = "%Y-%m-%d %H:%M:%S")

# Calculate time between hits (mins)
space2$datetimediff <- difftime(space2$datetime, lag(space2$datetime), units = "mins")

# Remove NAs
#space2 <- space2[!is.na(space2$datetimediff),]

# Change to numeric class
space2$datetimediff <- as.numeric(space2$datetimediff)

# # Subset out same data logger hits from the same individual
# # within 15 minutes
# space21 <- space2[1,]
# space2 <- space2[-1,]
# space2 <- space2[!(space2$ID == lag(space2$ID) &
#                      space2$uniq.loc == lag(space2$uniq.loc) &
#                      space2$datetimediff > 0 &
#                      space2$datetimediff <15),]
# 
# space2 <- rbind(space21, space2)

# Create a distance column
# Distance calculated between each row
space2 <- space2 %>%
  mutate(Distance = pointDistance(cbind(x.coord.1m, y.coord.1m), cbind(lag(x.coord.1m), lag(y.coord.1m)), lonlat = F))

# Calculate time difference in minutes between rows 
space3 <- space2 %>%
  mutate(differ = (datetime - lag(datetime))/60)

# Function that moves column orders to the last 
movetolast <- function(data, move) {
  data[c(setdiff(names(data), move), move)]
}

# Move focused columns for the last for inspection
space3 <- movetolast(space3, c("datetime", "ID", "uniq.loc", "Distance", "differ"))

# Function that shifts row of a particular column upwards
# and replaces the non-existant rows with 0
shift <- function(x, n){
  c(x[-(seq(n))], rep(0, n))
}

# Shift distance and time difference column upwards by 1
space3$differ <- shift(space3$differ, 1)
space3$Distance <- shift(space3$Distance, 1)

# Convert NAs to None
space3 <- space3 %>%
  mutate(ID = if_else(is.na(ID), "None", ID))

# Remove distance and time differences between individuals
# We only want values calculated from the same individual!

for (i in 1:nrow(space3)){ 
  if (i == 19722) { #32254
    print("All calculated inter-individual distance and time differences are converted to 0!")
    break
  } else if (space3$ID[i] != space3$ID[i+1]){
    space3$Distance[i] <- 0
    space3$differ[i] <- 0
  }
}

# Create a second distance column with 20 meters
# added for individuals with the same logger hits after 15 minutes

space3$Distance20 <- space3$Distance

for (i in 1:nrow(space3)){ 
  if (i == 19722) { #32254
    print("20 meters added to same data logger hits for each individual on the same night after the 15 minutes mark!")
    break
  } else if (space3$ID[i] == space3$ID[i+1] & space3$uniq.loc[i] == space3$uniq.loc[i+1]){
    if (space3$differ[i] >= 15){
      space3$Distance20[i] <- 20
    }
  }
}

# for (i in 1:nrow(space3)){ 
#   if (i == 19260) { #32254
#     print("20 meters added to same data logger hits for each individual on the same night!")
#     break
#   } else if (space3$ID[i] == space3$ID[i+1] & space3$uniq.loc[i] == space3$uniq.loc[i+1]){
#     space3$Distance20[i] <- 20
#   }
# }

################ Create new dataframes based on per night per ID ################

# Total logger hits per night per ID
new <- space3 %>%
  group_by(full.date, ID) %>%
  count(visit)

# Number of different logger hits per night per ID
Difflog <- space3 %>%
  group_by(full.date, ID) %>%
  count(uniq.loc)                     #Based on 3 different columns

# Add a column of 1s, to sum up the number of unique uniq.loc
# length(unique()) was not working
Difflog$numb <- 1

# Sum up number of unique data logger hits per night per ID
Difflog1 <- Difflog %>%
  group_by(full.date, ID) %>%
  summarise_at(vars(numb), sum)

# New dataframe with mean data logger hits in rhododendron
# per night per ID
rhodo <- space3 %>%                                        
  group_by(full.date, ID) %>%                         
  summarise_at(vars(rhododendron),              
               list(name = mean)) 

# New dataframe with mean data logger hits in cover
# per night per ID
cover <- space3 %>%                                        
  group_by(full.date, ID) %>%                         
  summarise_at(vars(cover),              
               list(name = mean)) 

# # New dataframe with mean data logger hits near logs
# # per night per ID
# loggs <- space3 %>%                                        
#   group_by(full.date, ID) %>%                         
#   summarise_at(vars(log.hits),              
#                list(name = mean))

# New dataframe with mean data logger hits in areas of logs+stump volume
# per night per ID
micro <- space3 %>%                                        
  group_by(full.date, ID) %>%                         
  summarise_at(vars(logstump),              
               list(name = mean))

# New dataframe with mean data logger hits in bamboo
# per night per ID
bamb <- space3 %>%                                        
  group_by(full.date, ID) %>%                         
  summarise_at(vars(bamboo),              
               list(name = mean)) 

# New dataframe of total distance travelled
# per night per ID
travel <- space3 %>%                                        
  group_by(full.date, ID) %>%                         
  summarise_at(vars(Distance),              
               list(name = sum))

# New dataframe of total distance travelled (with 20m added)
# per night per ID
travel20 <- space3 %>%                                        
  group_by(full.date, ID) %>%                         
  summarise_at(vars(Distance20),              
               list(name = sum))

# Convert time difference column to numeric
space3$differ <- as.numeric(space3$differ)

# New dataframe of total activity time
# per night per ID
timediff <- space3 %>%                                        
  group_by(full.date, ID) %>%                         
  summarise_at(vars(differ),              
               list(name = sum))

# Change column names of all the new data frames
names(timediff)[3] <- "Act_time"
names(travel)[3] <- "Distance"
names(travel20)[3] <- "Distance20"
names(new)[4] <- "Total_hits"
names(rhodo)[3] <- "Rhodo"
names(cover)[3] <- "cover"
names(bamb)[3] <- "Bamboo"
#names(loggs)[3] <- "Logs"
names(micro)[3] <- "Logstump"
names(Difflog1)[3] <- "Diff_logger"

############### Merge all columns together! ###############
tgt <- new %>%
  left_join(Difflog1, by = c("full.date", "ID")) %>%
  left_join(rhodo, by = c("full.date", "ID")) %>%
  left_join(micro, by = c("full.date", "ID")) %>%
  left_join(cover, by = c("full.date", "ID")) %>%
  left_join(bamb, by = c("full.date", "ID")) %>%
  left_join(travel, by = c("full.date", "ID")) %>%
  left_join(travel20, by = c("full.date", "ID")) %>%
  left_join(timediff, by = c("full.date", "ID")) 
  #left_join(loggs, by = c("full.date", "ID")) %>%

# Create a new column of spatiotemporal activity
# Distance/Activity time
tgt$Activity <- tgt$Distance/tgt$Act_time
tgt$Activity20 <- tgt$Distance20/tgt$Act_time

# Convert Nans to 0, Nans created by 0/0 from 1 data logger hits
tgt <- tgt %>%
  mutate_all(~replace(., is.nan(.), 0))

############ Add lunar illumination information ############

# Change date format
moon$Date <- format(as.Date(moon$Date, format = "%d/%m/%Y"), "%Y-%m-%d")
moon$Date <- as.Date(moon$Date)

# Match moon info by the date column
tgt$Moon <- moon$Illumination[match(tgt$full.date, moon$Date)]

######### Add gender and season info ##########
tgt$Sex <- space3$Sex[match(tgt$ID, space3$ID)]
tgt$season <- space3$season[match(tgt$full.date, space3$full.date)]
tgt$season.n <- space3$season.n[match(tgt$full.date, space3$full.date)]

# colSums(is.na(tgt)) shows 6 rows of NAs in Sex
# due to escaped mice, except M413
tgt <- tgt[!is.na(tgt$Sex),]

# # Convert the three other NAs to 0
# tgt <- tgt %>%
#   mutate_all(~replace(., is.na(.), 0))

# Remove the visit column
tgt$visit <- NULL

######### Add data logger and trapping effort ##########
tgt$datalog <- 0
for (i in 1:nrow(tgt)){
  if (tgt$full.date[i] < "2010-06-06"){
    tgt$datalog[i] <- 5
  } else {
    tgt$datalog[i] <- 10
  }
}
tgt$trap <- 0
for (i in 1:nrow(tgt)){
  if (tgt$full.date[i] < "2010-11-10"){
    tgt$trap[i] <- 1
  } else {
    tgt$trap[i] <- 0.5
  }
}

################ Export to Data directory ################
write.csv(tgt, "../Data/pernightperID7update.csv", row.names = F)


########### Add mean spaceuse to each individual ###########
space3$spaceuse <- space3$Distance20/space3$differ

space3$spaceuse[is.nan(space3$spaceuse)]<-0

space3$spaceuse[is.infinite(space3$spaceuse)]<-0

# Create a new dataframe of spaceuse for each individual
speedy <- space3 %>%
     group_by(ID) %>%
     summarise_at(vars(spaceuse),
                  list(name = mean))

# Match spaceuse to capturehistory data set
indi$spaceuse <- speedy$name[match(indi$ID, speedy$ID)]

################ Export to Data directory ################
write.csv(indi, "../Data/capturehistoryCOMB4.csv", row.names = F)
