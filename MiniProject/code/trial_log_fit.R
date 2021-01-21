#__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
#__version__ = '3.6.3'

######### Model fitting using logged data #########

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

# gompertz_model <- function(t, r_max, N_max, N_0, t_lag){ 
#   return(N_0 + (N_max - N_0) * exp(-exp(r_max * exp(1) * (t_lag - t)/((N_max - N_0) * log(10)) + 1)))
# }     

baranyi_model <- function(N_0, N_max, r_max, t, t_lag){ 
  return(N_max + log10((-1+exp(r_max*t_lag) + exp(r_max*t))/(exp(r_max*t) - 1 + exp(r_max*t_lag) * 10^(N_max-N_0))))
}

buchanan_model <- function(t, r_max, N_max, N_0, t_lag){ 
  return(N_0 + (t >= t_lag) * (t <= (t_lag + (N_max - N_0) * log(10)/r_max)) * r_max * (t - t_lag)/log(10) + (t >= t_lag) * (t > (t_lag + (N_max - N_0) * log(10)/r_max)) * (N_max - N_0))
}

## Initialize the results data frame ##
Stats50 <- data.frame(ID = character(), Quadratic_AIC = numeric(), 
                      Cubic_AIC = numeric(), Logistic_AIC = numeric(), 
                      Baranyi_AIC = numeric(), 
                      Buchanan_AIC = numeric(),
                      Quadratic_rsq = numeric(), Cubic_rsq = numeric(), 
                      Logistic_rsq = numeric(),
                      Baranyi_rsq = numeric(), Buchanan_rsq = numeric(),
                      stringsAsFactors = FALSE)

counter <- 1
## Initializing starting parameters, model fitting (linear and nonn-linear), calculating AIC, plotting!
for (i in unique(data$id2)){
  # For each unique data set
  supb <- subset(data, data$id2 == i)
  ID <- counter 
  
  # Ignore data sets with less than 4 data points
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
  
  ################ Fitting linear models ##################
  
  # Quadratic
  Qua_Fit <- lm(logpop1 ~ poly(Time, 2), data = supb)   
  QuaAIC <- AIC(Qua_Fit)
  Qua_rsq <- summary(Qua_Fit)$adj.r.squared
  
  # Cubic
  Cub_Fit <- lm(logpop1 ~ poly(Time, 3), data = supb) 
  CubAIC <- AIC(Cub_Fit)
  Cub_rsq <- summary(Cub_Fit)$adj.r.squared
  
  ########### Sampling starting values for non-linear models ###########
  
  # Set seed to make code reproducible
  set.seed(55)
  # Initialize a matrix with 100 nd samples of each predicted starting parameters
  N_0_start_lots <- matrix(rnorm(100, mean = N_0_start, sd = 0.2) , nrow = 100, ncol = 1)
  N_max_start_lots <- rnorm(100, mean = N_max_start, sd = 0.2)
  r_max_start_lots <- rnorm(100, mean = r_max_start, sd = 0.2)
  t_lag_start_lots <- rnorm(100, mean = t_lag_start, sd = 0.2)
  # Adding all into one matrix
  starting_val <- cbind(N_0_start_lots, N_max_start_lots, r_max_start_lots, t_lag_start_lots)
  colnames(starting_val) <- c("N_0_start", "N_max_start", "r_max_start", "t_lag_start")
  
  for (z in 1:nrow(starting_val)){
    ################ Fitting non-linear models ##################
    
    # Logistic
    Logis_Fit <- try(nlsLM(logpop1 ~ loglogistic_model(N_0, N_max, r_max, t = Time), 
                           data = supb, start = c(N_0 = unname(starting_val[z, 1]), 
                                                  N_max = unname(starting_val[z, 2]), 
                                                  r_max = unname(starting_val[z, 3])), 
                           control = nls.lm.control(maxiter = 100)), silent = T) 
    if (class(Logis_Fit) != 'try-error'){
      LogisAIC2 <- AIC(Logis_Fit)
      Logistic_rss <- sum(residuals(Logis_Fit)^2)
      Logistic_tss <- sum((supb$logpop1 - mean(supb$logpop1))^2)
      Logistic_rsq2 <- 1 - (Logistic_rss/Logistic_tss)
    } else {
      LogisAIC2 <- "NA"
      Logistic_rsq2 <- "NA"
    }
    
    
    # Make sure the first AIC and rsq values are set as a baseline for comparisons
    
    if (exists("LogisAIC") == FALSE && exists("Logistic_rsq") == FALSE){
      if (class(Logis_Fit) != 'try-error'){
        LogisAIC <- LogisAIC2
        Logistic_rsq <- Logistic_rsq2
        Logis_Fit_final <- Logis_Fit
      }
    }
    
    # If the newly sampled value has a better AIC and rsq values, replace
    if (exists("LogisAIC") == TRUE && exists("Logistic_rsq") == TRUE){
      if (LogisAIC2 < LogisAIC){
        LogisAIC <- LogisAIC2
        Logistic_rsq <- Logistic_rsq2
        Logis_Fit_final <- Logis_Fit
      }
    }
      
    # Gompertz, REMOVED
    # Gomp_Fit <- try(nlsLM(logpop1 ~ gompertz_model(N_0, N_max, r_max, t = Time, 
    #                                                t_lag), data = supb, 
    #                       start = c(N_0 = unname(starting_val[z, 1]), 
    #                                 N_max = unname(starting_val[z, 2]), 
    #                                 r_max = unname(starting_val[z, 3]), 
    #                                 t_lag = unname(starting_val[z, 4])), 
    #                       control = nls.lm.control(maxiter = 100)), silent = T) 
    # if (class(Gomp_Fit) != 'try-error'){
    #   GompertzAIC2 <- AIC(Gomp_Fit)
    #   Gompertz_rss <- sum(residuals(Gomp_Fit)^2)
    #   Gompertz_tss <- sum((supb$logpop1 - mean(supb$logpop1))^2)
    #   Gompertz_rsq2 <- 1 - (Gompertz_rss/Gompertz_tss)
    # } else {
    #   GompertzAIC2 <- "NA"
    #   Gompertz_rsq2 <- "NA"
    # }
    # 
    # # Make sure the first AIC and rsq values are set as a baseline for comparisons
    # 
    # if (exists("GompertzAIC") == FALSE && exists("Gompertz_rsq") == FALSE){
    #   if (class(Gomp_Fit) != 'try-error'){
    #     GompertzAIC <- GompertzAIC2
    #     Gompertz_rsq <- Gompertz_rsq2
    #     Gomp_Fit_final <- Gomp_Fit
    #   }
    # }
    # 
    # 
    # # If the newly sampled value has a better AIC and rsq values, replace
    # if (exists("GompertzAIC") == TRUE && exists("Gompertz_rsq") == TRUE){
    #   if (GompertzAIC2 < GompertzAIC){
    #     GompertzAIC <- GompertzAIC2
    #     Gompertz_rsq <- Gompertz_rsq2
    #     Gomp_Fit_final <- Gomp_Fit
    #     cat("Gomp", counter, "/n")
    #   }
    # }
    
    # Baranyi
    Bar_Fit <- try(nlsLM(logpop1 ~ baranyi_model(N_0, N_max, r_max, t = Time, 
                                                 t_lag), data = supb, 
                         start = c(N_0 = unname(starting_val[z, 1]), 
                                   N_max = unname(starting_val[z, 2]), 
                                   r_max = unname(starting_val[z, 3]), 
                                   t_lag = unname(starting_val[z, 4])), 
                         control = nls.lm.control(maxiter = 100)), silent = T) 
    if (class(Bar_Fit) != 'try-error'){
      BaranyiAIC2 <- AIC(Bar_Fit)
      Baranyi_rss <- sum(residuals(Bar_Fit)^2)
      Baranyi_tss <- sum((supb$logpop1 - mean(supb$logpop1))^2)
      Baranyi_rsq2 <- 1 - (Baranyi_rss/Baranyi_tss)
    } else {
      BaranyiAIC2 <- "NA"
      Baranyi_rsq2 <- "NA"
    }    
    
    # Make sure the first AIC and rsq values that are not NAs are set as a baseline for comparisons
    if (exists("BaranyiAIC") == FALSE && exists("Baranyi_rsq") == FALSE){
      if (BaranyiAIC2 != "NA" && Baranyi_rsq2 != "NA"){
        BaranyiAIC <- BaranyiAIC2
        Baranyi_rsq <- Baranyi_rsq2
        Bar_Fit_final <- Bar_Fit # Make sure model fit is saved if the first sampled value is the best
      }
    }
    
    
    # If the newly sampled value has a better AIC and rsq values, replace
    if (exists("BaranyiAIC") == TRUE && exists("Baranyi_rsq") == TRUE){
      if (BaranyiAIC2 < BaranyiAIC){
        BaranyiAIC <- BaranyiAIC2
        Baranyi_rsq <- Baranyi_rsq2
        Bar_Fit_final <- Bar_Fit
      }
    }

    
    # Buchnan
    Buch_Fit <- try(nlsLM(logpop1 ~ buchanan_model(N_0, N_max, r_max, t = Time, 
                                                 t_lag), data = supb, 
                         start = c(N_0 = unname(starting_val[z, 1]), 
                                   N_max = unname(starting_val[z, 2]), 
                                   r_max = unname(starting_val[z, 3]), 
                                   t_lag = unname(starting_val[z, 4])), 
                         control = nls.lm.control(maxiter = 100)), silent = T) 
    if (class(Buch_Fit) != 'try-error'){
      BuchananAIC2 <- AIC(Buch_Fit)
      Buchanan_rss <- sum(residuals(Buch_Fit)^2)
      Buchanan_tss <- sum((supb$logpop1 - mean(supb$logpop1))^2)
      Buchanan_rsq2 <- 1 - (Buchanan_rss/Buchanan_tss)
    } else {
      BuchananAIC2 <- "NA"
      Buchanan_rsq2 <- "NA"
    }    
    
    # Make sure the first AIC and rsq values that are not NAs are set as a baseline for comparisons
    if (exists("BuchananAIC") == FALSE && exists("Buchanan_rsq") == FALSE){
      if (BuchananAIC2 != "NA" && Buchanan_rsq2 != "NA"){
        BuchananAIC <- BuchananAIC2
        Buchanan_rsq <- Buchanan_rsq2
        Buch_Fit_final <- Buch_Fit # Make sure the model fit is saved if the first sampled value has the best fit
      }
    }
    
    # If the newly sampled value has a better AIC and rsq values, replace
    if (exists("BuchananAIC") == TRUE && exists("Buchanan_rsq") == TRUE){
      if (BuchananAIC2 < BuchananAIC){
        BuchananAIC <- BuchananAIC2
        Buchanan_rsq <- Buchanan_rsq2
        Buch_Fit_final <- Buch_Fit
      }
    }
  }
  ## If none of the sampled values can fit the data, fill with NA
  if (exists("BaranyiAIC") == FALSE && exists("Baranyi_rsq") == FALSE){
    BaranyiAIC <- "NA"
    Baranyi_rsq <- "NA"
  }
  
  if (exists("BuchananAIC") == FALSE && exists("Buchanan_rsq") == FALSE){
    BuchananAIC <- "NA"
    Buchanan_rsq <- "NA"
  }
  
  # Filling in the results data frame with calculated AIC and adj r-squared values
  stat_values <- c(ID, QuaAIC, CubAIC, LogisAIC, BaranyiAIC, BuchananAIC, Qua_rsq, 
             Cub_rsq, Logistic_rsq, Baranyi_rsq, Buchanan_rsq)
  Stats50[i, ] <- stat_values
  
  counter <- counter + 1
  
  ################## Creating the plots #####################
  # x-axis for the models
  length <- seq(from = min(supb$Time), to = max(supb$Time), length.out = 250)
  
  # Saving the predicted data points for each model and merging them all together
  Qp <- predict.lm(Qua_Fit, data.frame(Time = length))
  df1 <- data.frame(length, Qp)
  df1$Models <- "Quadratic"
  names(df1) <- c("length", "PopBio_pred", "Models")
  dfp <- df1
  
  if(CubAIC != "NA" && is.infinite(CubAIC) == FALSE){ # If the AIC is not NA and infinite
    Cp <- predict.lm(Cub_Fit, data.frame(Time = length))
    df2 <- data.frame(length, Cp)
    df2$Models <- "Cubic"
    names(df2) <- c("length", "PopBio_pred", "Models")
    dfp <- rbind(dfp, df2) # rbind the dataframes
  }
  if(LogisAIC != "NA" && is.infinite(LogisAIC) == FALSE){
    Lp <- loglogistic_model(t = length, r_max = coef(Logis_Fit_final)["r_max"], 
                         N_max = coef(Logis_Fit_final)["N_max"], 
                         N_0 = coef(Logis_Fit_final)["N_0"])
    df3 <- data.frame(length, Lp)
    df3$Models <- "Logistic"
    names(df3) <- c("length", "PopBio_pred", "Models")
    dfp <- rbind(dfp, df3)
  }
  
  # if(GompertzAIC != "NA" && is.infinite(GompertzAIC) == FALSE){
  #   Gop <- gompertz_model(t = length, r_max = coef(Gomp_Fit_final)["r_max"], 
  #                         N_max = coef(Gomp_Fit_final)["N_max"], 
  #                         N_0 = coef(Gomp_Fit_final)["N_0"], 
  #                         t_lag = coef(Gomp_Fit_final)["t_lag"])
  #   df4 <- data.frame(length, Gop)
  #   df4$Models <- "Gompertz"
  #   names(df4) <- c("length", "PopBio_pred", "Models")
  #   dfp <- rbind(dfp, df4)
  # }
  
  if(BaranyiAIC != "NA" && is.infinite(BaranyiAIC) == FALSE){
    Bp <- baranyi_model(t = length, r_max = coef(Bar_Fit_final)["r_max"], 
                        N_max = coef(Bar_Fit_final)["N_max"], 
                        N_0 = coef(Bar_Fit_final)["N_0"], 
                        t_lag = coef(Bar_Fit_final)["t_lag"])
    df5 <- data.frame(length, Bp)
    df5$Models <- "Baranyi"
    names(df5) <- c("length", "PopBio_pred", "Models")
    dfp <- rbind(dfp, df5)
  }
  if(BuchananAIC != "NA" && is.infinite(BuchananAIC) == FALSE){
    Bch <- buchanan_model(t = length, r_max = coef(Buch_Fit_final)["r_max"], 
                        N_max = coef(Buch_Fit_final)["N_max"], 
                        N_0 = coef(Buch_Fit_final)["N_0"], 
                        t_lag = coef(Buch_Fit_final)["t_lag"])
    df6 <- data.frame(length, Bch)
    df6$Models <- "Buchanan"
    names(df6) <- c("length", "PopBio_pred", "Models")
    dfp <- rbind(dfp, df6)
  }
  
  ## Plotting ##
  p <- ggplot(supb, aes(x = Time, y = logpop1)) +
    geom_point(size = 3) + 
    geom_line(data = dfp, aes(x = length, y = PopBio_pred, col = Models), size = 1.5) + # Model prediction lines
    scale_color_manual(values = c("#E69F00", "#009E73", "#D55E00", "#56B4E9", "#F0E442")) +
    theme_bw() + 
    theme(aspect.ratio = 1) + 
    labs(title = paste("ID number:", ID), x = ("Time (hrs)"), y = paste("log(Population) ", supb$PopBio_units[i], sep = "")) +
    theme(plot.title = element_text(size = 16, face = "bold"))

  ## Save plot in results
  png(paste("../results/", "Sampling", i, ".png", sep = ""), width=600, height=500, res=120) # start export
  print(p)
  dev.off() 
  
  ## Remove the best AIC, rsq values and model fits before the start of next loop
  rm(list = ls(pattern = "^Bar"))
  rm(list = ls(pattern = "^Logis"))
  rm(list = ls(pattern = "^Buch"))
}

## NOT INCLUDING GOMPERTZ MODEL
# The obtained AIC values do not reflect its actual fit on the graphs
# For many of my data sets, gomp_AIC values were the lowest but the visual fits were very very poor!

## Export stats results
write.csv(Stats50, "../results/logstats_sampling.csv")
