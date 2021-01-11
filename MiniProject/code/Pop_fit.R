#__author__ = 'Billy Lam (ykl17@ic.ac.uk)'
#__version__ = '3.6.3'

## Model fitting ##

## Load packages ##
library(ggplot2)
require("minpack.lm")
library(growthrates) # for extracting max growth rate, need deSolve and lattice


## Load data ##
data <- read.csv("../data/Pop_clean.csv", stringsAsFactors = F, header = T)


## Logistic equation, this is in linear scale ##
logistic_model <- function(t, r_max, N_max, N_0){ # The classic logistic equation
  return(N_0 * N_max * exp(r_max * t)/(N_max + N_0 * (exp(r_max * t) - 1))) 
}
# Log of logistic equation
loglogistic_model <- function(t, r_max, N_max, N_0){ # The classic logistic equation
  return(log(N_0 * N_max * exp(r_max * t)/(N_max + N_0 * (exp(r_max * t) - 1)))) 
}


########### Visualization for each unique ID ###############
plot_visual <- function(x) {
  counter <- 0
  
  for (i in unique(data$id2)){
    # Data subset
    supb <- subset(data, data$id2 == i)
    
    pp <- ggplot(supb, aes(x = Time, y = logc)) +
      geom_point() +
      theme_bw()
    
    print(pp + 
            ggtitle(paste(supb$Species[i],"\n", supb$Medium[i], "\n", "Unique ID:", supb$id2[i])) +
                          # substr(supb$Citation2[i], ########
                          #        start = 1, stop = 75))) + 
            xlab("Time (hrs)") + 
            ylab("Population"))
    
    # plot(supb$Time, supb$PopBio,
    #      main= paste(supb$Species[i],"\n", supb$Medium[i], "\n",
    #                  substr(supb$Citation[i], start = 1, stop = 50)), cex.main=1,
    #      ylab= "Population",
    #      xlab= "Time (hours)")
    
    counter <- counter + 1
    #print(paste("Created", counter, "plots."))
  }
  
}

#### Plot graph in sandbox ####
pdf(paste("../sandbox/", "triallog1", ".pdf")) ###############
plot_visual(df)
dev.off()

######### Plot linear model on pop ##########
plot_lm <- function(x) {
  counter <- 0
  
  for (i in unique(data$id2)){
    # Data subset
    supb <- subset(data, data$id2 == i)
    
    lm_result <- lm(logpop ~ Time, supb)
    
    pp <- ggplot(supb, aes(x = Time, y = logpop)) +
      geom_point() +
      theme_bw() +
      stat_smooth(method="lm", se=TRUE, fill=NA,
                  formula = y ~ poly(x, 3, raw=TRUE),colour="red")

    print(pp + 
            ggtitle(paste(supb$Species[i],"\n", supb$Medium[i], "\n", supb$Temp[i], "\n")) +
            # substr(supb$Citation2[i], ########
          #        start = 1, stop = 75))) + 
            xlab("Time (hrs)") +
            ylab("log(Population)"))

    counter <- counter + 1
    #print(paste("Created", counter, "plots."))
  }
  
}

#### Plot graph in sandbox ####
pdf(paste("../sandbox/", "trial_log_cubic", ".pdf")) ###############
plot_lm(df)
dev.off()


# if (any(duplicated(supb$Time) == TRUE)){
#   print ("a is TRUE")
# } else {
#   print ("a is FALSE")
# }

############# Model fitting with logistic model ###############
plot_logfit <- function(x) {
  i = 80
  counter <- 0
  
  for (i in unique(data$id2)){
    # Data subset
    supb <- subset(data, data$id2 == i)
    
    # check if got duplicated time points
    if (any(duplicated(supb$Time) == TRUE)){
      counter <- counter + 1
      print(paste("Skipping unique id", counter, "due to duplicated time points")); next # pass to next iteration as fit_easylinear won't work
    }
    else if (nrow(supb) < 6){
      counter <- counter + 1
      print(paste("Skipping unique id", counter, "due to small sample size")); next
    }
    else {
      
      # using data with no duplicated time points
      fit <- growthrates::fit_easylinear(supb$Time, supb$PopChange)
      sl <- unname(fit@par[3]) # use @ cuz fit is a S4 object, par shows exponential growth parameters
      
      # # lm to get ols estimates
      # lm_growth <- lm(PopBio ~ Time, supb)
      # sl <- unname(lm_growth$coefficients[2]) # unname for
      
      # # Generating starting parameters for the logistic model
      N_0_start <- min(supb$PopChange) # lowest population size, doesnt seem to work properly
      N_max_start <- max(supb$PopChange) # highest population size
      r_max_start <- sl # use our max slope estimate from fit_easylinear
      
      #Range1 <- range(supb$PopBio, na.rm = TRUE, finite=TRUE) 
      
      supb <- supb[supb$Time != 0, ] #subset out time points = 0
      
      # # Model fit
      fit_logistic <- nlsLM(PopChange ~ logistic_model(t = Time, r_max, N_max, N_0), supb,
                            list(r_max=r_max_start, N_0 = N_0_start, N_max = N_max_start))
      
      # fit_logistic <- nlsLM(PopBio ~ loglogistic_model(t = Time, r_max, N_max, N_0), supb,
      #                       list(r_max=r_max_start, N_0 = N_0_start, N_max = N_max_start))
      
      # # Make a data frame of the fitted data
      timepoints <- supb$Time
      logistic_points <- logistic_model(t = timepoints,
                                        r_max = coef(fit_logistic)["r_max"],
                                        N_max = coef(fit_logistic)["N_max"],
                                        N_0 = coef(fit_logistic)["N_0"])
      df1 <- data.frame(timepoints, logistic_points)
      df1$model <- "Logistic equation"
      names(df1) <- c("Time", "N", "model")
      
      # ggplot with fitted line
      pp <- ggplot(supb, aes(x = Time, y = PopChange)) +
        
        theme_bw()
      
      print(pp + 
              ggtitle(paste(supb$Species[i],"\n", supb$Medium[i], "\n", supb$Temp[i])) + 
              xlab("Time (hrs)") + 
              ylab("Population") +
              geom_line(data = df1, aes(x = Time, y = N, col = model), size = 1) +
              geom_point())
      
      counter <- counter + 1
      print(paste("Created", counter, "plots."))
    } #else
  }#for
  
}#func




# Print graph in sandbox
pdf(paste("../sandbox/", "triallogfitC", ".pdf"))
plot_logfit(df)
dev.off()




## Gompertz model
gompertz_model <- function(t, r_max, N_max, N_0, t_lag){ # Modified gompertz growth model (Zwietering 1990)
  return(N_0 + (N_max - N_0) * exp(-exp(r_max * exp(1) * (t_lag - t)/((N_max - N_0) * log(10)) + 1)))
}                   

############# Model fitting with gompertz model ###############
plot_gom <- function(x) {
  #i = 2
  counter <- 0
  
  for (i in unique(data$id2)){
    # Data subset
    supb <- subset(data, data$id2 == i)
    
    # check if got duplicated time points
    if (any(duplicated(supb$Time) == TRUE)){
      counter <- counter + 1
      print(paste("Skipping unique id", counter, "due to duplicated time points")); next # pass to next iteration as fit_easylinear won't work
    }
    else if (nrow(supb) < 6){
      counter <- counter + 1
      print(paste("Skipping unique id", counter, "due to small sample size")); next
    }
    else {
      
      # using data with no duplicated time points
      fit <- growthrates::fit_easylinear(supb$Time, supb$PopBio)
      sl <- unname(fit@par[3]) # use @ cuz fit is a S4 object, par shows exponential growth parameters
      yi <- unname(fit@par[2])
      
      # # lm to get ols estimates
      # lm_growth <- lm(PopBio ~ Time, supb)
      # sl <- unname(lm_growth$coefficients[2]) # unname for
      
      # # Generating starting parameters for the logistic model
      N_0_start <- min(supb$PopBio) # lowest population size, doesnt seem to work properly
      N_max_start <- max(supb$PopBio) # highest population size
      r_max_start <- sl # use our max slope estimate from fit_easylinear
      t_lag_start <- supb$Time[which.max(diff(diff(supb$PopBio)))] # find last timepoint of lag phase
      # t_lag_start <- (-yi)/sl
      #Range1 <- range(supb$PopBio, na.rm = TRUE, finite=TRUE) 
      
      supb <- supb[supb$Time != 0, ] #subset out time points = 0
      
      # # Model fit
      tryCatch({fit_gompertz <- nlsLM(PopBio ~ gompertz_model(t = Time, r_max, N_max, N_0, t_lag), data,
                                      list(t_lag=t_lag_start, r_max=r_max_start, N_0 = N_0_start, N_max = N_max_start))},error = function(e){print("error")})
      
      fit_gompertz <- nlsLM(PopBio ~ gompertz_model(t = Time, r_max, N_max, N_0, t_lag), data,
                            list(t_lag=t_lag_start, r_max=r_max_start, N_0 = N_0_start, N_max = N_max_start))
      
      # # Make a data frame of the fitted data
      timepoints <- supb$Time
      gompertz_points <- gompertz_model(t = timepoints, 
                                        r_max = coef(fit_gompertz)["r_max"], 
                                        N_max = coef(fit_gompertz)["N_max"], 
                                        N_0 = coef(fit_gompertz)["N_0"], 
                                        t_lag = coef(fit_gompertz)["t_lag"])
      df1 <- data.frame(timepoints, gompertz_points)
      df1$model <- "Gompertz model"
      names(df1) <- c("Time", "LogN", "model")
      
      # ggplot with fitted line
      pp <- ggplot(supb, aes(x = Time, y = PopBio)) +
        
        theme_bw()
      
      print(pp + 
              ggtitle(paste(supb$Species[i],"\n", supb$Medium[i], "\n", supb$Temp[i])) + 
              xlab("Time (hrs)") + 
              ylab("Population") +
              geom_line(data = df1, aes(x = Time, y = LogN, col = model), size = 1) +
              geom_point())
      
      counter <- counter + 1
      print(paste("Created", counter, "plots."))
    } #else
  }#for
  
}#func

# Print graph in sandbox
pdf(paste("../sandbox/", "gomp1", ".pdf"))
plot_gom(df)
dev.off()
