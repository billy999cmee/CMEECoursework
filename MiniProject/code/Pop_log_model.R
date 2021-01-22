#__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
#__version__ = '3.6.3'

## Model fitting using logged data ##
# PREDICT ONLY #
cat("Starting model fitting with predicted starting values, this might take a while...")

## Load necessary packages ##
library(ggplot2)
library(minpack.lm)

# An additional package could be "growthrates", it got a written function to extract rmax and tlag

## Load data ##
data <- read.csv("../data/Pop_clean.csv", stringsAsFactors = F, header = T)

#### Defining equations! ####
logistic_model <- function(N_0, N_max, r_max, t){
  return((N_0 * N_max * exp(r_max*t)) / (N_max + N_0 * (exp(r_max*t) - 1)))
}

loglogistic_model <- function(N_0, N_max, r_max, t){
  return(log((N_0 * N_max * exp(r_max*t)) / (N_max + N_0 * (exp(r_max*t) - 1))))
}

baranyi_model <- function(N_0, N_max, r_max, t, t_lag){ # Baranyi model (Baranyi 1993)
  return(N_max + log10((-1+exp(r_max*t_lag) + exp(r_max*t))/(exp(r_max*t) - 1 + exp(r_max*t_lag) * 10^(N_max-N_0))))
}

buchanan_model <- function(t, r_max, N_max, N_0, t_lag){ 
  return(N_0 + (t >= t_lag) * (t <= (t_lag + (N_max - N_0) * log(10)/r_max)) * r_max * (t - t_lag)/log(10) + (t >= t_lag) * (t > (t_lag + (N_max - N_0) * log(10)/r_max)) * (N_max - N_0))
}

## Initialize the results data frame ##
Stats25 <- data.frame(ID = character(), Quadratic_AIC = numeric(), 
                     Cubic_AIC = numeric(), Logistic_AIC = numeric(), 
                     Buchanan_AIC = numeric(), Baranyi_AIC = numeric(), 
                     Quadratic_rsq = numeric(), Cubic_rsq = numeric(), 
                     Logistic_rsq = numeric(), Buchanan_rsq = numeric(),
                     Baranyi_rsq = numeric(), stringsAsFactors = FALSE)


## Initializing starting parameters, model fitting (linear and nonn-linear), calculating AIC, plotting!
for (i in unique(data$id2)){
  # For each unique data set
  supb <- subset(data, data$id2 == i)
  
  if ((nrow(supb)<4) == TRUE) {next}
  
  ############## Initializing starting parameters ###############
  N_0_start <- min(supb$logpop1) # N0 = minimum PopBio
  N_max_start <- max(supb$logpop1) # Nmax = maximum PopBio
  
  # Generate initial regression for comparisons to find rmax in the while loop!
  a <- lm(supb$logpop1 ~ supb$Time) #Initial regression fit
  y_int <- a$coefficients[1] #Y-intercept of the initial regression fit
  r_max_start <- a$coefficients[2] #Gradient of initial regression fit
  
  # Setting conditions for the while loop so that it runs x (total number of rows -3) number of times for each data set
  j <- 1
  n <- dim(supb)[1] - 3 # Running lm for every 3 points in the data set, so the nrow(supb) - 3 is the number of times the lm will run
  rdata <- supb
  
  while(j < n){ # Prevent R trying to lm() over available data
    if(length(unique(rdata$Time)) != 1){
      rmaxx <- lm(rdata$logpop1[j:(j+4)]~rdata$Time[j:(j+4)])$coefficients[2]
      y_int2 <- lm(rdata$logpop1[j:(j+4)]~rdata$Time[j:(j+4)])$coefficients[1]
      j <- j + 1
      if(r_max_start < rmaxx){ # If new r is greater than old r
        r_max_start <- rmaxx # Make the the old rmax equal to the new rmax
        y_int <- y_int2 # Make the old y-intercept equal to the new y-intercept
      }
    } 
  }
  
  r_max_start <- as.numeric(r_max_start) 
  y_int <- as.numeric(y_int)
  t_lag_start <- (-y_int) / r_max_start # t_lag = x-intercept of the r_max tangent, so this calculates it
  
  
  ################ Fitting the models ##################
  ID <- supb$id2[i] #Getting the unique ID for each data set
  
  ## Linear ##
  # Quadratic
  Qua_Fit <- lm(logpop1 ~ poly(Time, 2), data = supb)   
  QuaAIC <- AIC(Qua_Fit)
  Qua_rsq <- summary(Qua_Fit)$adj.r.squared
  
  # Cubic
  Cub_Fit <- lm(logpop1 ~ poly(Time, 3), data = supb) 
  CubAIC <- AIC(Cub_Fit)
  Cub_rsq <- summary(Cub_Fit)$adj.r.squared
  
  ## Non-linear ##
  # Logistic
  Logis_Fit <- try(nlsLM(logpop1 ~ loglogistic_model(N_0, N_max, r_max, t = Time), 
                         data = supb, start = c(N_0 = N_0_start, 
                                                N_max = N_max_start, 
                                                r_max = r_max_start), 
                         control = nls.lm.control(maxiter = 100)), silent = T) 
  if (class(Logis_Fit) != 'try-error'){
    LogisAIC <- AIC(Logis_Fit)
    Logistic_rss <- sum(residuals(Logis_Fit)^2)
    Logistic_tss <- sum((supb$logpop1 - mean(supb$logpop1))^2)
    Logistic_rsq <- 1 - (Logistic_rss/Logistic_tss)
  } else {
    LogisAIC <- "NA"
    Logistic_rsq <- "NA"
  }
  
  # Buchanan
  Buch_Fit <- try(nlsLM(logpop1 ~ buchanan_model(N_0, N_max, r_max, t = Time, 
                                                t_lag), data = supb, 
                        start = c(N_0 = N_0_start, N_max = N_max_start, 
                                  r_max = r_max_start, t_lag = t_lag_start), 
                        control = nls.lm.control(maxiter = 100)), silent = T) 
  if (class(Buch_Fit) != 'try-error'){
    BuchananAIC <- AIC(Buch_Fit)
    Buchanan_rss <- sum(residuals(Buch_Fit)^2)
    Buchanan_tss <- sum((supb$logpop1 - mean(supb$logpop1))^2)
    Buchanan_rsq <- 1 - (Buchanan_rss/Buchanan_tss)
  } else {
    BuchananAIC <- "NA"
    Buchanan_rsq <- "NA"
  }
  
  # Baranyi
  Bar_Fit <- try(nlsLM(logpop1 ~ baranyi_model(N_0, N_max, r_max, t = Time, 
                                              t_lag), data = supb, 
                       start = c(N_0 = N_0_start, N_max = N_max_start, 
                                 r_max = r_max_start, t_lag = t_lag_start), 
                       control = nls.lm.control(maxiter = 100)), silent = T) 
  if (class(Bar_Fit) != 'try-error'){
    BaranyiAIC <- AIC(Bar_Fit)
    Baranyi_rss <- sum(residuals(Bar_Fit)^2)
    Baranyi_tss <- sum((supb$logpop1 - mean(supb$logpop1))^2)
    Baranyi_rsq <- 1 - (Baranyi_rss/Baranyi_tss)
  } else {
    BaranyiAIC <- "NA"
    Baranyi_rsq <- "NA"
  }    
  
  # Filling in the results data frame with calculated AIC and adj r-squared values
  whtev <- c(ID, QuaAIC, CubAIC, LogisAIC, BuchananAIC, BaranyiAIC, Qua_rsq, 
             Cub_rsq, Logistic_rsq, Buchanan_rsq, Baranyi_rsq)
  Stats25[i, ] <- whtev
  
  
  ################## Creating the plots #####################
  # x-axis for the models
  length <- seq(from = min(supb$Time), to = max(supb$Time), length.out = 250)

  # Saving the predicted data points for each model and merging them all together
  Qp <- predict.lm(Qua_Fit, data.frame(Time = length))
  df1 <- data.frame(length, Qp)
  df1$Model <- "Quadratic"
  names(df1) <- c("length", "PopBio_pred", "Model")
  dfp <- df1

  if(CubAIC != "NA" && is.infinite(CubAIC) == FALSE){ # If the AIC is not NA and infinite
    Cp <- predict.lm(Cub_Fit, data.frame(Time = length))
    df2 <- data.frame(length, Cp)
    df2$Model <- "Cubic"
    names(df2) <- c("length", "PopBio_pred", "Model")
    dfp <- rbind(dfp, df2) # rbind the dataframes
  }
  if(LogisAIC != "NA" && is.infinite(LogisAIC) == FALSE){
    Lp <- loglogistic_model(t = length, r_max = coef(Logis_Fit)["r_max"], N_max = coef(Logis_Fit)["N_max"], N_0 = coef(Logis_Fit)["N_0"])
    df3 <- data.frame(length, Lp)
    df3$Model <- "Logistic"
    names(df3) <- c("length", "PopBio_pred", "Model")
    dfp <- rbind(dfp, df3)
  }
  if(BuchananAIC != "NA" && is.infinite(BuchananAIC) == FALSE){
    Gop <- buchanan_model(t = length, r_max = coef(Buch_Fit)["r_max"], N_max = coef(Buch_Fit)["N_max"], N_0 = coef(Buch_Fit)["N_0"], t_lag = coef(Buch_Fit)["t_lag"])
    df4 <- data.frame(length, Gop)
    df4$Model <- "Buchanan"
    names(df4) <- c("length", "PopBio_pred", "Model")
    dfp <- rbind(dfp, df4)
  }
  if(BaranyiAIC != "NA" && is.infinite(BaranyiAIC) == FALSE){
    Bp <- baranyi_model(t = length, r_max = coef(Bar_Fit)["r_max"], N_max = coef(Bar_Fit)["N_max"], N_0 = coef(Bar_Fit)["N_0"], t_lag = coef(Bar_Fit)["t_lag"])
    df5 <- data.frame(length, Bp)
    df5$Model <- "Baranyi"
    names(df5) <- c("length", "PopBio_pred", "Model")
    dfp <- rbind(dfp, df5)
  }


  ## Plotting ##
  p <- ggplot(supb, aes(x = Time, y = logpop1)) +
    geom_point(size = 3) + #Data point size
    geom_line(data = dfp, aes(x = length, y = PopBio_pred, col = Model), size = 1.5) + # Model prediction lines
    scale_color_manual(values = c("#E69F00", "#009E73", "#D55E00", "#56B4E9", "#F0E442")) +
    theme_bw() + # Black and white background
    theme(aspect.ratio = 1) + # Square background
    labs(title = paste("ID number:", ID), x = ("Time (hrs)"), y = paste("log(Population) ", supb$PopBio_units[1], sep = "")) +
    theme(plot.title = element_text(size = 16, face = "bold"))

  ## Save plot in results
  png(paste("../results/", "Predict", i, ".png", sep = ""), width=600, height=500, res=120) # start export
  print(p)
  dev.off() # finish export
}

## Export stats results
write.csv(Stats25, "../results/logstats_predict.csv")
