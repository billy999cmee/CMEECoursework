#__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
#__version__ = '3.6.3'

## Model analysis

# Load packages
library(ggplot2)

# Read results 
res <- read.csv("../data/logstats2.csv", stringsAsFactors = F)

# Data subsetting for clear interpretations, splitting rsquared and ACIs
rsqq <- res[,8:12]
aics <- res[,3:7]

# Replace Inf in data by NA for aics
my_data <- do.call(data.frame,
                   lapply(aics,
                          function(x) replace(x, is.infinite(x), NA)))

# Find which model has the lowest aic, r-squared by counting showing the column of the lowest value for each row
sum_rsq <- apply(rsqq, 1, which.max)
sum_aic <- apply(aics, 1, which.min)


# Which model has the best fit for rsquared?
# sum(is.na(rsqq$Quadratic_rsq)) #5
# sum(sum_rsq == 1, na.rm = T) #176, log = 187
# sum(is.na(rsqq$Cubic_rsq)) #10
# sum(sum_rsq == 2, na.rm = T) #51, log = 35
# sum(sum_rsq == 1)/(sum(sum_rsq == 1)+sum(sum_rsq == 2)) #0.77


aicfit <- data.frame(Models, result)

# aicfit1 <- t(aicfit)
# colnames(aicfit1) <- lapply(aicfit1[1, ], as.character)
# aicfit1 <- unname(aicfit1[-1,]) 
# barplot(as.numeric(aicfit1))


####### Calculating the number of times a model being the best fit #######
# Initialize scores for counting
Qua_score <- 0
Cub_score <- 0
Log_score <- 0
Bara_score <- 0
Buch_score <- 0

# There are 3 possible combinations of AIC, both aics negative/positive and 
# best_aic is negative and second best aic is positive
# If it is the 3rd combination, then abs(best_aic) - abs(second) will not give the correct answer,
# therefore it will be calculated by: second best aic - 2 should be < best aic value
# For example, best aic: -2, second best aic: 0.6
# 0.6 minus 2 is not less than -2, hence, the difference is greater than 2!

# Loop to calculate number of best fits using AIC and delta AIC scores
for (i in 1:nrow(my_data)){
  # The lowest AIC value and its corresponding column name
  best <- names(my_data[i, ])[which.min(apply(my_data[i, ], MARGIN=2, min))]
  best_aic <- sort(my_data[i, ], FALSE)[1]
  
  # Record the lowest AIC
  if (best == "Quadratic_AIC"){
    Qua_score <- Qua_score + 1
  } else if (best == "Cubic_AIC"){
    Cub_score <- Cub_score + 1
  } else if (best == "Logistic_AIC"){
    Log_score <- Log_score + 1
  } else if (best == "Baranyi_AIC"){
    Bara_score <- Bara_score + 1
  } else if (best == "Buchanan_AIC"){
    Buch_score <- Buch_score + 1
  }
  
  # Check whether there is a tie between the best and second best AIC value
  # The second lowest AIC value
  second <- sort(my_data[i, ], FALSE)[2]
  
  #### 3 overall if statements for the 3 combinations of positives and negatives ####
  
  if (best_aic < 0 && second < 0){ # If both negative
    if (abs(best_aic) - abs(second) < 2){
      if (colnames(second) == "Quadratic_AIC"){
        Qua_score <- Qua_score + 1
      } else if (colnames(second) == "Cubic_AIC"){
        Cub_score <- Cub_score + 1
      } else if (colnames(second) == "Logistic_AIC"){
        Log_score <- Log_score + 1
      } else if (colnames(second) == "Baranyi_AIC"){
        Bara_score <- Bara_score + 1
      } else if (colnames(second) == "Buchanan_AIC"){
        Buch_score <- Buch_score + 1
      }
    }
  } else if (best_aic > 0 && second > 0){ # If both positive
    if (best_aic - second < 2){
      if (colnames(second) == "Quadratic_AIC"){
        Qua_score <- Qua_score + 1
      } else if (colnames(second) == "Cubic_AIC"){
        Cub_score <- Cub_score + 1
      } else if (colnames(second) == "Logistic_AIC"){
        Log_score <- Log_score + 1
      } else if (colnames(second) == "Baranyi_AIC"){
        Bara_score <- Bara_score + 1
      } else if (colnames(second) == "Buchanan_AIC"){
        Buch_score <- Buch_score + 1
      }
    }
  } else if (best_aic < 0 && second > 0){ # If best aic is negative and second best is positive
    if (second - 2 < best_aic){
      if (colnames(second) == "Quadratic_AIC"){
        Qua_score <- Qua_score + 1
      } else if (colnames(second) == "Cubic_AIC"){
        Cub_score <- Cub_score + 1
      } else if (colnames(second) == "Logistic_AIC"){
        Log_score <- Log_score + 1
      } else if (colnames(second) == "Baranyi_AIC"){
        Bara_score <- Bara_score + 1
      } else if (colnames(second) == "Buchanan_AIC"){
        Buch_score <- Buch_score + 1
      }
    }
  }
}

# Transfer results into a dataframe
aicfit <- data.frame(c("Quadratic", "Cubic", "Logistic", "Baranyi", "Buchanan"),
                     c(Qua_score, Cub_score, Log_score, Bara_score, Buch_score))
names(aicfit)[1] <- "Models"
names(aicfit)[2] <- "No_best_fits"

# Plotting graph displaying number of times a model was better, relatively, using AIC
p <- ggplot(data = aicfit, aes(x = Models, y = No_best_fits, fill = Models)) +
  geom_bar(stat = "identity") +
  labs(title = "Model selection based on AIC and delta AIC scores", y = "Number of best AIC scores") +
  theme(aspect.ratio = 1) +
  theme_bw() +
  theme_classic() +
  theme(plot.title = element_text(size = 16, face = "bold"))
p

#### Plot graph in result ####
pdf(paste("../results/", "ModelselectionAIC", ".pdf")) ###############
p
dev.off()


####### Shows the percentage of model fitted to the data, by calculating ######
Models <- c("Quadratic", "Cubic", "Logistic", "Baranyi", "Buchanan")
Percentage_fitted <- c() 
for (i in 1:ncol(my_data)){
  p <- sum(!is.na(my_data[i]))/nrow(my_data)
  #print(p)
  Percentage_fitted <- append(Percentage_fitted, p)
}

# Bind into a data frame
dat1 <- data.frame(Models, Percentage_fitted)
# tab1 <- rbind(Models, Percentage_fitted)
# colnames(tab1) <- tab1[1,] #copy row1 to column headers

# Save as a table into results directory
write.table(dat1, "../results/tab1", col.names = T, row.names = F)

####### Calculate the number of times R2 was the highest for each model ######
# Initialize for appending results
Models2 <- c("Baranyi", "Cubic", "Buchanan", "Quadratic", "Logistic")
result <- c()

for (i in unique(sum_rsq)){
  p <- sum(sum_rsq == i, na.rm = T)
  print(paste(i, "=", p))
  result <- append(result, p)
}

# Bind into a data frame
dat2 <- data.frame(Models2, result)

# Save as a table into results directory
write.table(dat2, "../results/table_r2", col.names = T, row.names = F)