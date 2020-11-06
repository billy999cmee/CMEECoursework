#__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
#__version__ = '3.6.3'

# Body mass distributions practical

# Generating subplots
MyDF <- read.csv("../data/EcolArchives-E089-51-D1.csv")

# View data
str(MyDF)
unique(MyDF$Type.of.feeding.interaction)#so there will be 5 subplots for each

# Predator mass subplots using a for loop
pdf("../results/Pred_Subplots.pdf")
par(mfcol=c(5,1))
par(mar=c(4,5,0.9,3)) # trial and error results
for (i in unique(MyDF$Type.of.feeding.interaction)) {
  subseted_df = subset(MyDF, Type.of.feeding.interaction == i)
  hist(log(subseted_df$Predator.mass), 
       xlab="Body Mass (g)", ylab="Count", col = "blue", 
       main = paste0(i, " Predator mass distribution")) 
}
graphics.off();

# Prey mass subplots
pdf("../results/Prey_Subplots.pdf")
par(mfcol=c(5,1))
par(mar=c(4,5,0.9,3)) # trial and error results
for (i in unique(MyDF$Type.of.feeding.interaction)) {
  subseted_df = subset(MyDF, Type.of.feeding.interaction == i)
  hist(log(subseted_df$Prey.mass), 
       xlab="Body Mass (g)", ylab="Count", col = "lightgreen", 
       main = paste0(i, " Prey mass distribution")) 
}
graphics.off();

# Make a new column of prey mass/predator mass
MyDF$Ratio <- MyDF$Prey.mass/MyDF$Predator.mass
# Pred prey size ratio subplots
pdf("../results/SizeRatio_Subplots.pdf")
par(mfcol=c(5,1))
par(mar=c(4,5,0.9,3)) # trial and error results
for (i in unique(MyDF$Type.of.feeding.interaction)) {
  subseted_df = subset(MyDF, Type.of.feeding.interaction == i)
  hist(log(subseted_df$Ratio), 
       xlab="Body Mass (g)", ylab="Count", col = "yellow", 
       main = paste0(i, " Prey/Predator mass ratio distribution")) 
}
graphics.off();

## Making a new data frame with no data input
new <- data.frame(Mass_origin=character(), Feeding_type=character(), 
                  Mean=numeric(), Median=numeric(), stringsAsFactors = F)

# Predator mass
for (i in unique(MyDF$Type.of.feeding.interaction)) {
  subseted_df = subset(MyDF, Type.of.feeding.interaction == i)
  new[i, 1] = "Predator"
  new[i, 2] = i
  new[i, 3] = mean(log(subseted_df$Predator.mass))
  new[i, 4] = median(log(subseted_df$Predator.mass))
}

# Prey mass
new1 <- data.frame(Mass_origin=character(), Feeding_type=character(), 
                  Mean=numeric(), Median=numeric(), stringsAsFactors = F)
for (i in unique(MyDF$Type.of.feeding.interaction)) {
  subseted_df = subset(MyDF, Type.of.feeding.interaction == i)
  new1[i, 1] = "Prey"
  new1[i, 2] = i
  new1[i, 3] = mean(log(subseted_df$Prey.mass))
  new1[i, 4] = median(log(subseted_df$Prey.mass))
}
#Append prey to pred to make a new data frame
newnew <- rbind(new, new1)

# Prey/Pred ration
new2 <- data.frame(Mass_origin=character(), Feeding_type=character(), 
                   Mean=numeric(), Median=numeric(), stringsAsFactors = F)
for (i in unique(MyDF$Type.of.feeding.interaction)) {
  subseted_df = subset(MyDF, Type.of.feeding.interaction == i)
  new2[i, 1] = "Prey/Pred_ratio"
  new2[i, 2] = i
  new2[i, 3] = mean(log(subseted_df$Ratio))
  new2[i, 4] = median(log(subseted_df$Ratio))
}
#Append ratio to pred+prey and make a new dataframe
newnewnew <- rbind(newnew, new2)

#Export to results
write.csv(newnewnew, "../results/PP_Results.csv", row.names = FALSE)

