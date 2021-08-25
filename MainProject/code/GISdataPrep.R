################ GIS ################

## Vector Data

# Load libraries
library(rgdal)
library(ggplot2)
#library(sf) for logs vicinity only
#library(rgeos) for logs vicinity only
library(dplyr)

# taRifx.geo download instruction for the latest version from github
# install.packages("remotes")
# remotes::install_github("gsk3/taRifx.geo")
library(taRifx.geo) #lineDist function

# Read csv with trap and logger coordinates
space <- read.csv("../Data/MASTERSPACE (17.01.15, study day inc, for BL).csv")

# Save the shapefiles directory
dir1 <- "C:/Users/billy/Desktop/Master/Data/Shapes"

# Read all shapefiles, dsn = must specify a specific directory!
rodo <- readOGR(dsn = dir1, layer = "RODODENDRON_POL")
bamboo <- readOGR(dsn = dir1, layer = "BAMBOO_POL")
bluebellc <- readOGR(dsn = dir1, layer = "BLUEBELL_COV") #Doesn't contribute
bluebellp <- readOGR(dsn = dir1, layer = "BLUEBELL_POL")
bracken <- readOGR(dsn = dir1, layer = "BRACKEN_COV2")
debris <- readOGR(dsn = dir1, layer = "DEBRIS_POL")
logs <- readOGR(dsn = dir1, layer = "LOGS_LIN")
nobrack <- readOGR(dsn = dir1, layer = "NOBRACKEN_POL")
stump <- readOGR(dsn = dir1, layer = "STUMP_PNT")
treee <- readOGR(dsn = dir1, layer = "TREES_PNT")


######### Data wrangling #########
# New ID column based on uniq.loc, for point sizes
space$pt.size <- cumsum(!duplicated(space$uniq.loc))

# New ID column based on row number for matching purposes later
space$row.numb <- 1:nrow(space)

# Make a duplicate for spatialpointdf conversion later
space1 <- space


######### Visual plot of all the layers #########
ggplot()+
  geom_polygon(data = rodo, aes(x = lat, y = long, group = group, fill = "Rhododendron")) +
  geom_polygon(data = bamboo, aes(x = lat, y = long, group = group, fill = "Bamboo")) +
  geom_polygon(data = bluebellp, aes(x = lat, y = long, group = group, fill = "Bluebell")) +
  geom_polygon(data = bracken, aes(x = lat, y = long, group = group, fill = "Bracken")) +
  geom_path(data = logs, aes(x = lat, y = long, group = group, colour = "Fallen logs")) +
  scale_fill_manual(values = c("green", "seagreen3", "blue", "red")) +
  ggtitle("Nash's Copse") +
  theme_bw() +
  labs(fill="Patches", colour="Lines")

# Visualise covers (rhodo and bamboo) + microhabitat (logs and stumps)
stump1 <- data.frame(stump)

ggplot()+
  geom_polygon(data = rodo, aes(x = lat, y = long, group = group, fill = "Rhododendron")) +
  geom_polygon(data = bamboo, aes(x = lat, y = long, group = group, fill = "Bamboo")) +
  geom_point(data = stump1, aes(x = Y, y = X, size = DIAMM), shape = 1, colour = "purple") +
  geom_path(data = logs, aes(x = lat, y = long, group = group, colour = "Fallen logs")) +
  scale_fill_manual(values = c("green", "seagreen3", "blue", "red")) +
  ggtitle("Nash's Copse") +
  theme_bw() +
  ylab("Longitude") +
  xlab("Latitude") +
  labs(fill="Patches", colour="Lines") +
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

# 90 degrees anticlockwise of the plot above and save in pdf

pdf("../Results/SI1.pdf")

ggplot()+
  geom_polygon(data = rodo, aes(x = long, y = lat, group = group, fill = "Rhododendron")) +
  geom_polygon(data = bamboo, aes(x = long, y = lat, group = group, fill = "Bamboo")) +
  geom_point(data = stump1, aes(x = X, y = Y, size = DIAMM), shape = 1, colour = "purple") +
  geom_path(data = logs, aes(x = long, y = lat, group = group, colour = "Fallen logs")) +
  scale_fill_manual(values = c("green", "seagreen3", "blue", "red")) +
  ggtitle("Study site at Nash's Copse") +
  theme_bw() +
  labs(fill="Patches", colour="Lines") +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(), # x label text
        axis.text.y=element_blank(), # y label text
        axis.ticks=element_blank(), # x and y ticks
        axis.title.x=element_blank(), # x label
        axis.title.y=element_blank(), # y label
        # legend.position="none", # legend labels
        panel.background=element_blank(),
        panel.border=element_blank(), # box
        # panel.grid.major=element_blank(), # grid lines
        # panel.grid.minor=element_blank(), # grid lines
        plot.background=element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"), # legend box
        plot.title = element_text(size = 40, face = "bold")) + 
  scale_y_continuous(minor_breaks = seq(0, 130, 10)) +
  scale_x_continuous(minor_breaks = seq(0, 190, 10))

dev.off()


# Map of Nash's Copse 
# Habitat characteristics : Rhododendron, bamboo and logs
# Data logger locations
ggplot()+
  geom_polygon(data = rodo, aes(x = long, y = lat, group = group, fill = "Rhododendron")) + 
  geom_polygon(data = bamboo, aes(x = long, y = lat, group = group, fill = "Bamboo")) + 
  geom_path(data = logs, aes(x = long, y = lat, group = group, colour = "Logs")) +
  geom_point(data = space, aes(x = x.coord.1m, y = y.coord.1m, size = pt.size), 
            shape = 4, colour = "purple") + 
  scale_fill_manual(values = c("light blue", "seagreen3")) +
  scale_color_manual(values = "black") +
  guides(fill = guide_legend(reverse=TRUE)) + # Change legend order
  ggtitle("Nash's Copse") +
  theme_bw() +
  labs(fill="Patches", colour = "", size = "Sizes") 


######### Matching coordinates to habitat types #########

### Rhododendron and bamboo ###

# Create a spatial points data frame 
coordinates(space1) = ~x.coord.1m+y.coord.1m

# Set the projection to be the same, as we use the same coordinate system
proj4string(space1) = proj4string(rodo)

## Rhododendron and bamboos!!
# Make a data frame with hits in rhododendron and bamboo
rhod <- over(space1, rodo)
bambb <- over(space1, bamboo)

# Cbind new columns to space and rename columns
space <- cbind(space, bambb)
colnames(space)[46] <- "bamboo"

space <- cbind(space, rhod)
colnames(space)[47] <- "rhododendron"

# Convert NAs to 0
space$bamboo[is.na(space$bamboo)] <- 0
space$rhododendron[is.na(space$rhododendron)] <- 0

# Convert all non-zero numeric values to 1
space$bamboo[space$bamboo > 0] <- 1
space$rhododendron[space$rhododendron > 0] <- 1

### Cover = rhodo + bamboo
space$cover <- space$bamboo + space$rhododendron

#################### For log hits only!! ####################

#################### Not needed anymore, decided to use logstump volume! ####################

### Check which traps are within 1 meters from a fallen log ###

# # Make a geometry list (class sf) of space and logs
# log.list <- st_as_sf(space, coords = c("x.coord.1m", "y.coord.1m"))
# logs.sf <- st_as_sf(logs)
# 
# # Find which data logger hits are on logs or 1m within logs
# # Will take a while!
# sel <- st_is_within_distance(logs.sf, log.list, dist = 1)
# 
# # Extrarct all numbers from the list
# trial1 <- unique(unlist(split(sel, " "), " "))
# 
# # Match rownumber to logger hits next to logs
# space$log.hits <- trial1[match(space$row.numb, trial1)]
# #which(space$row.numb %in% trial1)
# 
# # Convert NAs to 0
# space$log.hits[is.na(space$log.hits)] <- 0
# 
# # Convert all non-zero numeric values to 1
# space$log.hits[space$log.hits > 0] <- 1

############## Micrhohabitat volume calculations ##############

### Fallen logs + stumps ###
### Log volume = pi*r^2l, stump voume = pi*r^2h

# Calculate fallen logs length (l)
logs1 <- lineDist(logs)
logs1 <- data.frame(logs1)

# Create log volume column using the formula
logs1$logvolume <- pi*logs1$distances*((logs1$DIAMM/2)^2)

# Stump volum already calculated in the original dataset

# Create new dataframes of logs and stump volume 
# for each quadrat
logsvol <- logs1 %>%                                        
  group_by(SQ) %>%                         
  summarise_at(vars(logvolume),              
               list(name = sum)) 

stumpvol <- stump1 %>%
  group_by(SQ) %>%
  summarise_at(vars(VOLM),              
               list(name = sum))

# Change column names for merging
names(logsvol)[2] <- "logsvol"
names(stumpvol)[2] <- "stumpvol"

# Merge the two columns by SQ
logstump <- logsvol %>% full_join(stumpvol, by = "SQ")

# Replace NAs with 0
logstump[is.na(logstump)] <- 0

# Create a new column with both added together
logstump$logstump <- logstump$logsvol + logstump$stumpvol

# Add logstump volume column to space
space$logstump <- logstump$logstump[match(space$grid.square, logstump$SQ)]

# Replace NAs with 0 for the logstump column
space$logstump[is.na(space$logstump)] <- 0

######### Export space to Data directory #########
write.csv(space, "../Data/gishabitats.csv", row.names = F)





######### Extra codes to plot graphs for hits only #########

# Check whether any data logger hits intersect with logs
#sel1 <- st_intersects(log.list, logs.sf, sparse = F)
#any(sel1 == T) # none intersects!!

# # Subset out hits only
# space.rhod.hit <- space.rhod[!is.na(space.rhod$Id),]
# 
# # Subet log hits
# space.loghitting <- space[space$log.hits == 1,]
# 
# # Verify all hits are in Rhododendron
# ggplot()+
#   geom_polygon(data = rodo, aes(x = long, y = lat, group = group, fill = "Rhododendron")) + 
#   geom_point(data = space[14966,], aes(x = x.coord.1m, y = y.coord.1m), 
#              shape = 4, colour = "purple") + 
#   scale_fill_manual(values = c("light blue", "seagreen3")) +
#   scale_color_manual(values = "black") +
#   guides(fill = guide_legend(reverse=TRUE)) + # Change legend order
#   ggtitle("Nash's Copse") +
#   theme_bw() +
#   labs(fill="Patches", colour = "") 
# 
# ggplot()+
#   geom_polygon(data = rodo, aes(x = long, y = lat, group = group, fill = "Rhododendron")) + 
#   geom_path(data = logs, aes(x = long, y = lat, group = group, colour = "black")) +
#   geom_point(data = space.loghitting, aes(x = x.coord.1m, y = y.coord.1m), 
#              shape = 4, colour = "red") + 
#   scale_fill_manual(values = c("light blue", "seagreen3")) +
#   scale_color_manual(values = "black") +
#   guides(fill = guide_legend(reverse=TRUE)) + # Change legend order
#   ggtitle("Nash's Copse") +
#   theme_bw() +
#   labs(fill="Patches", colour = "") 