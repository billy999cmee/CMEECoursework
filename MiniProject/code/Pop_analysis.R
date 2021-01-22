#__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
#__version__ = '3.6.3'

######### Model analysis #########

cat("Starting model analysis")

# Load packages
library(ggplot2)

# Read results for predict and sampling
samp_data <- read.csv("../results/logstats_sampling.csv", stringsAsFactors = F)
pred_data <- read.csv("../results/logstats_predict.csv", stringsAsFactors = F)

# Data subsetting for clear interpretations, splitting rsquared and ACIs
rsqq <- samp_data[,8:12]
aics <- samp_data[,3:7]

rsqq2 <- pred_data[,8:12]
aics2 <- pred_data[,3:7]

# Replace Inf in data by NA for aics
my_data_samp <- do.call(data.frame,
                   lapply(aics,
                          function(x) replace(x, is.infinite(x), NA)))

my_data_pred <- do.call(data.frame,
                        lapply(aics2,
                               function(x) replace(x, is.infinite(x), NA)))

# Swap columns for easy interpretations
my_data_pred <- my_data_pred[, c(1, 2, 3, 5, 4)]

# Find which model has the lowest aic, r-squared by counting showing the column of the lowest value for each row
sum_rsq_samp <- apply(rsqq, 1, which.max)
sum_rsq_pred <- apply(rsqq2, 1, which.max)


####### Calculating the number of times a model being the best fit for SAMPLING #######
# Initialize scores for counting
Qua_score <- 0
Cub_score <- 0
Log_score <- 0
Bara_score <- 0
Buch_score <- 0
counter <- 0 # Count number of data sets with 2 "best-fits"

# There are 3 possible combinations of AIC values, both aics negative/positive or 
# best_aic is negative and second best aic is positive
# If it is the 3rd combination, then abs(best_aic) - abs(second) will not give the correct answer,
# therefore it will be calculated by: second best aic - 2 should be < best aic value
# For example, best aic: -2, second best aic: 0.6
# abs(-2) - abs(0.6) is less than 2, tho in reality this is not true!
# Correct method: second best aic - 2 < best aic

# Loop to calculate number of best fits using AIC and delta AIC scores
for (i in 1:nrow(my_data_samp)){
  # The lowest AIC value and its corresponding column name
  best <- names(my_data_samp[i, ])[which.min(apply(my_data_samp[i, ], MARGIN=2, min))]
  best_aic <- sort(my_data_samp[i, ], FALSE)[1]
  
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
  second <- sort(my_data_samp[i, ], FALSE)[2]
  
  #### 3 overall if statements for the 3 combinations of positives and negatives ####
  
  if (best_aic < 0 && second < 0){ # If both negative
    if (abs(best_aic) - abs(second) < 2){
      counter <- counter + 1
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
      counter <- counter + 1
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
      counter <- counter + 1
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
aicfit <- data.frame("Models" = c("Quadratic", "Cubic", "Logistic", "Baranyi", "Buchanan"),
                    "AIC_best_score" = c(Qua_score, Cub_score, Log_score, Bara_score, Buch_score),
                    stringsAsFactors = F)

####### Calculating the number of times a model being the best fit for PREDICTIVE #######
# Initialize scores for counting
Qua_score <- 0
Cub_score <- 0
Log_score <- 0
Bara_score <- 0
Buch_score <- 0
counter <- 0

# Loop to calculate number of best fits using AIC and delta AIC scores
for (i in 1:nrow(my_data_pred)){
  # The lowest AIC value and its corresponding column name
  best <- names(my_data_pred[i, ])[which.min(apply(my_data_pred[i, ], MARGIN=2, min))]
  best_aic <- sort(my_data_pred[i, ], FALSE)[1]
  
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
  # When i == 158, only quadratic successfully converged
  if (i == 158){
    Qua_score <- Qua_score + 1
    cat("Skipping i=158")
    next
  } else {
    second <- sort(my_data_pred[i, ], FALSE)[2]
  }
  
  #### 3 overall if statements for the 3 combinations of positives and negatives ####
  
  if (best_aic < 0 && second < 0){ # If both negative
    if (abs(best_aic) - abs(second) < 2){
      counter <- counter + 1
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
      counter <- counter + 1
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
      counter <- counter + 1
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
aicfit_pred <- data.frame("Models" = c("Quadratic", "Cubic", "Logistic", "Baranyi", "Buchanan"),
                     "AIC_best_score" = c(Qua_score, Cub_score, Log_score, Bara_score, Buch_score),
                     stringsAsFactors = F)

# Bind sampling aic and predict aic together for plotting
total <- rbind(aicfit_pred, aicfit)
total$Methods <- c(rep("Predict (module 1)", 5), rep("Sampling (module 2)", 5)) # New categorical column for multi-column bar chart plotting

# Plotting graph displaying number of times a model was better, relatively, using AIC
p <- ggplot(data = total, aes(x = Models, y = AIC_best_score, fill = Methods)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Model selection based on AIC and delta AIC scores \n between two methods", y = "Number of best AIC scores") +
  scale_fill_manual("Methods", values = c("#88CCEE", "#117733")) +
  theme(aspect.ratio = 1) +
  theme_bw() +
  theme_classic() +
  theme(plot.title = element_text(size = 16, face = "bold"))
p

# Statistical test to compare AIC scores between the two modules
cat("Overall AIC comparison between 2 modules:")
kruskal.test(aicfit$AIC_best_score ~ aicfit_pred$AIC_best_score)

#### Plot graph in result ####
pdf(paste("../results/", "AICcompare", ".pdf", sep = "")) ###############
p
dev.off()

# Kruskal's test for AIC comparisons
cat("Kruskal-Wallis test between module 1 and 2:")
for (i in 1:5){
  end1 <- kruskal.test(my_data_pred[, i] ~ my_data_samp[, i])
  print(end1)
}

####### Calculate convergence success for sampling ######
Models <- c("Quadratic", "Cubic", "Logistic", "Baranyi", "Buchanan")
Percentage_fitted <- c() 
for (i in 1:ncol(my_data_samp)){
  p <- sum(!is.na(my_data_samp[i]))/nrow(my_data_samp)
  #print(p)
  Percentage_fitted <- append(Percentage_fitted, p)
}

# Bind into a data frame
dat1 <- data.frame(Models, Percentage_fitted)

# Save as a table into results directory
write.table(dat1, "../results/Convergence_success_table_sampling", col.names = T, row.names = F)

####### Calculate convergence success for prediction ######
Models_pred <- c("Quadratic", "Cubic", "Logistic", "Baranyi", "Buchanan")
Percentage_fitted <- c() 
for (i in 1:ncol(my_data_pred)){
  g <- sum(!is.na(my_data_pred[i]))/nrow(my_data_pred)
  #print(p)
  Percentage_fitted <- append(Percentage_fitted, g)
}

# Bind into a data frame
dat_pred <- data.frame(Models_pred, Percentage_fitted)

# Save as a table into results directory
write.table(dat_pred, "../results/Convergence_success_table_predict", col.names = T, row.names = F)

####### Calculate the number of times R2 was the highest for each model - sampling ######
# Initialize for appending results
Models2 <- c("Baranyi", "Cubic", "Buchanan", "Quadratic", "Logistic")
result <- c()

for (i in unique(sum_rsq_samp)){
  p <- sum(sum_rsq_samp == i, na.rm = T)
  #print(paste(i, "=", p))
  result <- append(result, p)
}

# Bind into a data frame
dat2 <- data.frame(Models2, "R2" = result)

# Save as a table into results directory
write.table(dat2, "../results/table_r2_sampling", col.names = T, row.names = F)

####### Calculate the number of times R2 was the highest for each model - predict ######
# Initialize for appending results
Models2_pred <- c("Baranyi", "Cubic", "Quadratic", "Buchanan", "Logistic")
result_pred <- c()

for (i in unique(sum_rsq_pred)){
  p <- sum(sum_rsq_pred == i, na.rm = T)
  #print(paste(i, "=", p))
  result_pred <- append(result_pred, p)
}

# Bind into a data frame
dat2_pred <- data.frame(Models2_pred, "R2" = result_pred)

# Save as a table into results directory
write.table(dat2_pred, "../results/table_r2_predict", col.names = T, row.names = F)