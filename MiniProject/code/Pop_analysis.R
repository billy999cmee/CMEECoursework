#__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
#__version__ = '3.6.3'

## Model analysis

# Load packages
library(ggplot2)

# Read results 
res <- read.csv("../data/logstats.csv", stringsAsFactors = F)

# Data subsetting for clear interpretations, splitting rsquared and ACIs
rsqq <- res[,8:9]
aics <- res[,3:7]
# Replace Inf in data by NA
my_data <- do.call(data.frame,                      
                   lapply(aics,
                          function(x) replace(x, is.infinite(x), NA)))

# Find which model has the lowest aic, r-squared by counting showing the column of the lowest value for each row
sum_rsq <- apply(rsqq, 1, which.min)
sum_aic <- apply(my_data, 1, which.min)

## Find how many times a model has the best AIC
# Each model is numbered :
# 1 = quadratic, 2 = cubic, 3 = logistic, 4 = gompertz, 5 = baranyi

# Which model has the best fit for rsquared?
sum(is.na(rsqq$Quadratic_rsq)) #5
sum(sum_rsq == 1, na.rm = T) #176, log = 187
sum(is.na(rsqq$Cubic_rsq)) #10
sum(sum_rsq == 2, na.rm = T) #51, log = 35
sum(sum_rsq == 1)/(sum(sum_rsq == 1)+sum(sum_rsq == 2)) #0.77

# Which model has the most number of "best fits"?
Models <- c("Cubic", "Logistic", "del", "Gompertz", "Baranyi", "Quadratic")

result <- c()
for (i in unique(sum_aic)){
  p <- sum(sum_aic == i, na.rm = T)
  print(paste(i, "=", p))
  result <- append(result, p)
}
#1 = 28, 2 = 62, 3 = 10, 4 = 118, 5 = 9
# 18, 37, 47, 74, 46

aicfit <- data.frame(Models, result)
aicfit <- aicfit[-3,] #remove 0 generated from the loop
# aicfit1 <- t(aicfit)
# colnames(aicfit1) <- lapply(aicfit1[1, ], as.character)
# aicfit1 <- unname(aicfit1[-1,]) 
# barplot(as.numeric(aicfit1))

p<-ggplot(data = aicfit, aes(x = Models, y = result, fill = Models)) +
  geom_bar(stat = "identity") +
  labs(title = "Model selection based on AIC scores", y = "Number of best AIC scores") +
  theme(aspect.ratio = 1, plot.title = element_text(size = 18,face = "bold"))
p

#### Plot graph in result ####
pdf(paste("../results/", "ModelselectionAIC", ".pdf")) ###############
p
dev.off()


# Shows the percentage of model fitted to the data, by calculating
Models <- c("Cubic", "Quadratic", "Logistic", "Gompertz", "Baranyi")
Percentage_fitted <- c() 
for (i in 1:ncol(my_data)){
  p <- sum(!is.na(my_data[i]))/nrow(my_data)
  #print(p)
  Percentage_fitted <- append(Percentage_fitted, p)
}
# 1, 0.9735, 0.2819, 0.82378, 0.22907
# 0.9779736, 0.9559471, 0.9427313,  0.6563877, 0.5330396

# Bind into a table
dat1 <- data.frame(Models, Percentage_fitted)
# tab1 <- rbind(Models, Percentage_fitted)
# colnames(tab1) <- tab1[1,] #copy row1 to column headers

write.table(dat1, "../results/tab1", col.names = T, row.names = F)
