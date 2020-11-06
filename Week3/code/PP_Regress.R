#__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
#__version__ = '3.6.3'

# Visualizing Regression analyses
# REALLY MESSSY

# Load data
MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")

# Inspect data
str(MyDF)
unique(MyDF$Predator.lifestage) #6 life stages

# Load packages
library(ggplot2)

# Function to generate the plot
myfun <- function(df){
  #Do ggplot
  p <- ggplot(df, aes(x = log(Predator.mass), y = log(Prey.mass), 
                      colour = Predator.lifestage, asp = 3))
  q <- p + theme_bw() + geom_point(shape=I(3)) + 
    facet_grid(Type.of.feeding.interaction ~.) + #grid for 1 column
    xlab("Prey Mass in grams") +
    ylab("Predator Mass in grams") +
    theme(legend.position = "bottom") + 
    geom_smooth(method = "lm", fullrange = TRUE) + 
    coord_fixed(ratio = 0.09) +
    ylim(-25, 20)
  print(q)
}
myfun(MyDF)

#Save in the result directory as a pdf file
pdf("../results/PP_Regress_Plot.pdf")

myfun(MyDF)

#For loop
#for (i in unique(MyDF$Type.of.feeding.interaction)){
  #subseted_df <- subset(MyDF, Type.of.feeding.interaction == i)
  #par(mfcol=c(5,1))
  #myfun(subseted_df)
#}
graphics.off()

# subset feeding interaction and split into 5 data frames
feed_split <- split(MyDF, MyDF$Type.of.feeding.interaction) #split dataset to list
list2env(feed_split, envir= .GlobalEnv) #split list to separate data sets
predpisc <- `predacious/piscivorous` #the / creates problems

#function to print summary table
fun1<-function(x){
  res<-c(x$coefficients[1],
         x$coefficients[2],
         summary(x)$r.squared,
         summary(x)$fstatistic,
         pf(summary(x)$fstatistic[1],summary(x)$fstatistic[2],
            summary(x)$fstatistic[3],lower.tail=FALSE))
  names(res)<-c("intercept","slope","r.squared",
                "F-statistic","numdf","dendf","p.value")
  return(res)}   

## lm for every life stage of feeding interaction
#insectivorous
counter <- 0
for ( i in unique(insectivorous$Predator.lifestage) ){
  #create a subset data 
  data_sub <- subset(insectivorous, Predator.lifestage == i)
  
  counter <- counter + 1
  #create the linear model. If it is the first loop,
  #then the model name will be lm_ins1
  j <- assign(paste("lm_ins",counter,sep = ""), lm(Prey.mass ~ Predator.mass, data_sub))
  
  #show many lms created
  print(paste("Created lm_ins",counter,sep = "")) 
  
  assign(paste("ins", counter, sep = ""), fun1(j))
}

#piscivorous
counter <- 0
for ( i in unique(piscivorous$Predator.lifestage) ){
  #create a subset data 
  data_sub <- subset(piscivorous, Predator.lifestage == i)
  
  counter <- counter + 1
  #create the linear model. If it is the first loop,
  #then the model name will be lm_pisc1
  j <- assign(paste("lm_pisc", counter,sep = ""), lm(Prey.mass ~ Predator.mass, data_sub))
  
  #show many lms created
  print(paste("Created lm_pisc", counter,sep = "")) 
  
  assign(paste("pisc", counter, sep = ""), fun1(j))
}

#planktivorous
counter <- 0
for ( i in unique(planktivorous$Predator.lifestage) ){
  #create a subset data 
  data_sub <- subset(planktivorous, Predator.lifestage == i)
  
  counter <- counter + 1
  #create the linear model. If it is the first loop,
  #then the model name will be lm_plank 1
  j <- assign(paste("lm_plank", counter,sep = ""), lm(Prey.mass ~ Predator.mass, data_sub))
  
  #show many lms created
  print(paste("Created lm_plank", counter,sep = "")) 
  
  assign(paste("plank", counter, sep = ""), fun1(j))
}

#predacious
counter <- 0
for ( i in unique(predacious$Predator.lifestage) ){
  #create a subset data 
  data_sub <- subset(predacious,Predator.lifestage == i)
  
  counter <- counter + 1
  #create the linear model. If it is the first loop,
  #then the model name will be lm_pred 1
  j <- assign(paste("lm_pred", counter,sep = ""), lm(Prey.mass ~ Predator.mass, data_sub))
  
  #show many lms created
  print(paste("Created lm_pred", counter,sep = "")) 
  
  assign(paste("pred", counter, sep = ""), fun1(j))
}

#predacious/piscivorous
counter <- 0
for ( i in unique(predpisc$Predator.lifestage) ){
  #create a subset data 
  data_sub <- subset(predpisc,Predator.lifestage == i)
  
  counter <- counter + 1
  #create the linear model. If it is the first loop,
  #then the model name will be lm_prepis 1
  j <- assign(paste("lm_prepis", counter,sep = ""), lm(Prey.mass ~ Predator.mass, data_sub))
  
  #show many lms created
  print(paste("Created lm_prepis", counter,sep = "")) 
  
  assign(paste("prepis", counter, sep = ""), fun1(j))
}



# bind by columns
re1 <- cbind(ins1, pisc1, pisc2, pisc3, pisc4, pisc5, plank1, plank2, plank4,
             plank5, pred1, pred2, pred3, pred4, pred5, pred6, prepis1)

#delete unnecessary rows and transpose
res22 <- re1[-c(5,6), ]
re2 <- t(res22)

write.csv(re2, "../results/PP_Regress_Results.csv", row.names = T)
