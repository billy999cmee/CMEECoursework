#__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
#__version__ = '3.6.3'

# Mapping practical

# Load data
load("../data/GPDDFiltered.RData") #gpdd

# Load map package
library(maps)
library(ggplot2)
library(rworldmap)

# World map using the maps package
maps::map(database = "world")

# World map using rworldmap package
world <- getMap(resolution = "low")

(with_world <- ggplot() +
  geom_polygon(data = world, 
               aes(x = long, y = lat, group = group),
               fill = NA, colour = "black") + 
  geom_point(data = gpdd,  # Add and plot species data
             aes(x = long, y = lat, color = "red")) +
  coord_quickmap() +  # Prevents stretching when resizing
  theme_classic() +  # Remove ugly grey background
  xlab("Longitude") +
  ylab("Latitude") +
  theme(legend.position = "none")) #supresses legend 

## The provided species data (gpdd) mainly includes species in Europe, Canada 
## and the United States of America. Also, species in the USA and Canada
## are mainly distributed in the coastline/country boundaries. But most 
## importantly, there are more data (rows) on certain species than other 
## species. Therefore, this data will create a bias in the results of spatial 
## analyses of species distribution as the data contains more individuals of
## certain species, meaning that those species will have more of an effect on 
## the analyzed results than species with only one row.

