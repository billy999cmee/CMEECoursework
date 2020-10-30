###############################################################################
################## Wrangling the Pound Hill Dataset using Tidyverse############
###############################################################################

############# Load the dataset ###############
# could use read_csv to avoid setting stringsAsFactors=F
#header=T to avoid an extra first column of numbers
MyData <- read.csv("../data/PoundHillData.csv",header = T)

# header = true because we do have metadata headers
MyMetaData <- read.csv("../data/PoundHillMetaData.csv",header = T, sep=";", stringsAsFactors = F)

########### Load tidyverse ############
require(tidyverse)
# the include_self = TRUE means list "tidyverse" as well 
tidyverse_packages(include_self = TRUE) 

############# Inspect the dataset ###############
head(MyData)
dim(MyData) # 45 60
dplyr::glimpse(MyData) 

########### Transpose data ###########
#pipe operator: %>%, allows stringing multiple functions together
MyData <- MyData %>%
  tibble::rownames_to_column() %>%  
  pivot_longer(-rowname) %>% 
  pivot_wider(names_from=rowname, values_from=value) #spread()

#Convert to a data frame
MyData <- as.data.frame(MyData, stringsAsFactors = F)

############# Replace species absences with zeros ###############
#From factor to character
MyData <- MyData %>% mutate_all(as.character)

#From empty string to NA
MyData <- MyData %>% mutate_all(list(~na_if(.,"")))

#From NA to 0
MyData <- MyData %>% 
                 mutate(across(everything(), ~replace_na(.x, 0)))
############# Make the first row as coolumn names ###############
#Dplyr option
names(MyData) <- MyData %>% slice(1) %>% unlist()
MyData <- MyData %>% slice(-1)

############# Convert from wide to long format  ###############
#Two new columns - Species and Count, from column 5 to 45
MyWrangledData <- MyData %>% 
  gather(Species, Count, 5:45) 


#Assigning the correct data types to each column
MyWrangledData <- MyWrangledData %>% mutate(
  Cultivation = factor(Cultivation), Block = factor(Block), Plot = factor(Plot), Quadrat = factor(Quadrat),
  Count = as.integer(Count)
)
dim(MyWrangledData)

############# Exploring the data (extend the script below)  ###############
#Convert data frame to a tibble
MyWrangledData <- tibble::as_tibble(MyWrangledData) 

#Inspect data, like str()
dplyr::glimpse(MyWrangledData) 

#Subset
dplyr::filter(MyWrangledData, Count>100) 

#Look at an arbitrary set of data rows
dplyr::slice(MyWrangledData, 10:15)
