################ Data Wrangling ################

# Load libraries
library(dplyr)
require(tidyr)
library(reshape)

# Read data
cap <- read.csv("../Data/captures.csv")
trap <- read.csv("../Data/Trappingcsv.csv")
space <- read.csv("../Data/gishabitats.csv")
moon <- read.csv("../Data/LunarIllumination.csv")

# Subset to only include Apodemus sylvaticus
AS <- subset(cap, Species == "AS")
trapAS <- subset(trap, Spcs == "AS")
spaceAS <- subset(space, Species == "AS")

################ Add lunar illumination to cap ################

# Change date format
moon$Date <- format(as.Date(moon$Date, format = "%d/%m/%Y"), "%Y-%m-%d")
moon$Date <- as.Date(moon$Date)

# Remove NAs in the year column
cap <- cap[!is.na(cap$year),]

# Change year format, 10 to 2010, 11 to 2011 etc..
AS$year <- gsub(10, 2010, AS$year)
AS$year <- gsub(11, 2011, AS$year)
AS$year <- gsub(12, 2012, AS$year)
AS$year <- gsub(13, 2013, AS$year)

cap$year <- gsub(10, 2010, cap$year)
cap$year <- gsub(11, 2011, cap$year)
cap$year <- gsub(12, 2012, cap$year)
cap$year <- gsub(13, 2013, cap$year)

spaceAS$year <- gsub(10, 2010, spaceAS$year)
spaceAS$year <- gsub(11, 2011, spaceAS$year)
spaceAS$year <- gsub(12, 2012, spaceAS$year)
spaceAS$year <- gsub(13, 2013, spaceAS$year)

# Form a date column in cap
AS$dates <- as.Date(with(AS, paste(year, month, day, sep = "-")),"%Y-%m-%d")
cap$dates <- as.Date(with(cap, paste(year, month, day, sep = "-")),"%Y-%m-%d")
spaceAS$dates <- as.Date(with(spaceAS, paste(year, month, day, sep = "-")),"%Y-%m-%d")

# Match lunar illumination to dates
cap$moon <- moon$Illumination[match(cap$dates, moon$Date)]
spaceAS$moon <- moon$Illumination[match(spaceAS$dates, moon$Date)]

# New dataframe with each individual's mean exposure to moon light
lunarsp <- spaceAS %>%                                        
  group_by(ID) %>%                         
  summarise_at(vars(moon),              
               list(name = mean)) 

lunarcap <- cap %>%                                        
  group_by(ID) %>%                         
  summarise_at(vars(moon),              
               list(name = mean)) 

################ Find avg weight based on ID ################

# Remove special characters for easy ID matching between trap and AS
trap$ID. <- gsub("-", "", trap$ID.)
trapAS$ID. <- gsub("-", "", trapAS$ID.)
trapAS$Mouse.weight <- gsub(",", ".", trapAS$Mouse.weight)
trapAS$Mouse.weight <- gsub(" ", "", trapAS$Mouse.weight)

# Convert weight to numeric
trapAS$Mouse.weight <- as.numeric(trapAS$Mouse.weight)

# Remove NAs based on mouseweight for mean calculation
trapAS <- trapAS[!is.na(trapAS$Mouse.weight),]

# New dataframe with each individual's mean weight
test <- trapAS %>%                                        
  group_by(ID.) %>%                         
  summarise_at(vars(Mouse.weight),              
               list(name = mean))  

################ Avg hits in different habitat types ################

# New dataframe with each individual's mean 
# data logger hits in rhododendron
rhod <- space %>%                                        
  group_by(ID) %>%                         
  summarise_at(vars(rhododendron),              
               list(name = mean)) 

# New dataframe with each individual's mean 
# data logger hits in bamboo
bamb <- space %>%                                        
  group_by(ID) %>%                         
  summarise_at(vars(bamboo),              
               list(name = mean)) 

# New dataframe with each individual's mean 
# data logger hits in cover
cover <- space %>%                                        
  group_by(ID) %>%                         
  summarise_at(vars(cover),              
               list(name = mean))

# # New dataframe with each individual's mean 
# # data logger hits next to logs (<=1m)
# loggy <- space %>%                                        
#   group_by(ID) %>%                         
#   summarise_at(vars(log.hits),              
#                list(name = mean)) 

# New dataframe with each individual's mean 
# data logger hits in areas of logs+stump volume
micro <- space %>%                                        
  group_by(ID) %>%                         
  summarise_at(vars(logstump),              
               list(name = mean))


################ Create pivot table based on ID ################

## Creating two datasets for experimental purposes...
## 1 - only trapping data (AS), 2 - trapping + data logger data (spaceAS)

# Arrange rows in order based on dates
AS <- AS[order(AS$dates),]
spaceAS <- spaceAS[order(spaceAS$dates),]

# Function that moves specific column to the last order
movetolast <- function(data, move) {
  data[c(setdiff(names(data), move), move)]
}

AS <- movetolast(AS, c("visit"))
spaceAS <- movetolast(spaceAS, "visit")

## Add in dates in between missing encounters for both data frames
hh <- data.frame(date=seq(as.Date("2010-03-28"), as.Date("2013-03-27"), by="days"))
h <- data.frame(date=seq(as.Date("2010-03-28"), as.Date("2013-03-15"), by="days"))

# Merge dataframe to identify the missing dates
ASfull <- merge(AS, hh, by.x='dates', by.y='date', all.x=T, all.y=T)
spaceASfull <- merge(spaceAS, h, by.x='dates', by.y='date', all.x=T, all.y=T)

ASfull$visit[is.na(ASfull$visit)]<-0
ASfull$ID[is.na(ASfull$ID)]<-"ignore"

spaceASfull$visit[is.na(spaceASfull$visit)]<-0
spaceASfull$ID[is.na(spaceASfull$ID)]<-"ignore"

# Make a new column for date order numbers
# AS$dates1 <- 0

# Make date order number, THIS IS VERY INEFFICIENT
# for (i in 1:nrow(AS)){
#   if (AS$dates[i] == AS$dates[1]){
#     AS$dates1[i] <- 1
#     j <- 1
#   } else if (AS$dates[i-1] == AS$dates[i]){
#     AS$dates1[i] <- j
#   } else if (AS$dates[i-1] < AS$dates[i]){
#     AS$dates1[i] <- j + 1
#     j <- j + 1
#   }
# }



#Turn into matrixes to ID in relation to date and sex
captab = cast(ASfull, ID ~ dates)
pivtab = cast(spaceASfull, ID ~ dates)

# Remove the first row
captab = captab[-c(1:2),]
pivtab = pivtab[-c(1:16),]

# Convert all non-zero numeric values to 1
captab2 <- captab %>% mutate_if(is.numeric, ~1 * (. != 0))
pivtab2 <- pivtab %>% mutate_if(is.numeric, ~1 * (. != 0))

# Create a capture history column as a string
df23 <- captab2 %>% unite(ch, 2:1097, sep = "", remove = T)
df22 <- pivtab2 %>% unite(ch, 2:1086, sep = "", remove = T)

# Match sex and mean covariates to individual ID
df22$sex <- trap$Sex[match(df22$ID, trap$ID.)]
df22$mweight <- test$name[match(df22$ID, test$ID.)]
df22$rhodo <- rhod$name[match(df22$ID, rhod$ID)]
df22$bamboo <- bamb$name[match(df22$ID, bamb$ID)]
df22$cover <- cover$name[match(df22$ID, cover$ID)]
#df22$logs <- loggy$name[match(df22$ID, loggy$ID)]
df22$logstump <- micro$name[match(df22$ID, micro$ID)]
df22$moon <- lunarcap$name[match(df22$ID, lunarcap$ID)]

df23$sex <- trap$Sex[match(df23$ID, trap$ID.)]
df23$mweight <- test$name[match(df23$ID, test$ID.)]
df23$rhodo <- rhod$name[match(df23$ID, rhod$ID)]
df23$bamboo <- bamb$name[match(df23$ID, bamb$ID)]
df23$cover <- cover$name[match(df23$ID, cover$ID)]
#df23$logs <- loggy$name[match(df23$ID, loggy$ID)]
df23$logstump <- micro$name[match(df23$ID, micro$ID)]
df23$moon <- lunarcap$name[match(df23$ID, lunarcap$ID)]

# Check number of NAs in each column
colSums(is.na(df22))
colSums(is.na(df23))
# which(is.na(df2$mouseweight))

# Remove NAs due to individuals not being AS, no record of M500
df22 <- na.omit(df22)
df23 <- na.omit(df23)

# Bin df2$moon to 4 levels, 0-25, 25-50, 50-75, 75-100
df22$moonbin <- cut(df22$moon, breaks = c(0, 25, 50, 75, 100), 
                    labels = c(1, 2, 3, 4), right = F)
df23$moonbin <- cut(df23$moon, breaks = c(0, 25, 50, 75, 100), 
                    labels = c(1, 2, 3, 4), right = F)

################ Export to Data directory ################
write.csv(df22, "../Data/capturehistoryCOMB4.csv", row.names = F)
write.csv(df23, "../Data/capturehistory4.csv", row.names = F)




###### Not needed, logger effort
# sumvisit <- space %>%                                        
#   group_by(season.n, ID) %>%                         
#   summarise_at(vars(visit),              
#                list(name = sum)) 
# 
# 
# for(i in 1:nrow(sumvisit)){
#   if (sumvisit$season.n[i] == 1){
#     sumvisit$box[i] <- 7.5
#   } else  {
#     sumvisit$box[i] <- 10
#   }
# }


## Checking if data violates CJS model assumption

# # Full data
# df44 <- df33$ch %>%
#   strsplit('') %>%
#   sapply(`[`) %>%
#   t() %>%
#   unlist() %>%
#   as.numeric %>%
#   matrix(nrow = nrow(df33))
# 
# overall_CJS(df4, rep(1, nrow(df3)))
# 
# # Females only
# df4ff <- df3$ch[df3$sex == "F"] %>%
#   strsplit('') %>%
#   sapply(`[`) %>%
#   t() %>%
#   unlist() %>%
#   as.numeric %>%
#   matrix(nrow = nrow(df3[df3$sex == "F",]))
# 
# # Males only
# df4mm <- df3$ch[df3$sex == "M"] %>%
#   strsplit('') %>%
#   sapply(`[`) %>%
#   t() %>%
#   unlist() %>%
#   as.numeric %>%
#   matrix(nrow = nrow(df3[df3$sex == "M",]))
# 
# overall_CJS(df4mm, rep(1, nrow(df3[df3$sex == "M",])))
# overall_CJS(df4ff, rep(1, nrow(df3[df3$sex == "F",])))
# 
# test2ct1 <- test2ct(df4, rep(1, nrow(df3)))
# test2ct1
# test2ct2 <- test2ct(df44, rep(1, nrow(df33)))
# test2ct2