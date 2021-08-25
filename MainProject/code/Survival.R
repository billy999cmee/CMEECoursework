################ Survival Analysis ################

# Cleanup files
# rm(list=ls(all=TRUE))
# cleanup(ask = FALSE)

# Load libraries
library(RMark)
library(ggplot2)

# Read data
df3 <- read.csv("../Data/capturehistory4.csv", 
                colClasses = c(ch = "character", sex = "factor", moonbin = "factor"))

################ RMark analysis ################

# Set markpath object to the current wd
MarkPath="C:/Users/billy/Desktop/"

# Remove ID column
df3 <- df3[,-1]

#df3 <- subset(df3, totalhits > 5)

#### Process data to CJS foramt ####

df3.processed = process.data(df3, model="CJS", groups= c("sex"))

# Create default design data (.ddl)
# Not necessary, but good practice, will provide useful info
df3.ddl = make.design.data(df3.processed) 


################ Create models ################
#my_first_model<-mark(df3.processed, df3.ddl)

# Extract results
#coef(my_first_model)
#str(summary(my_first_model))

# rm(my_first_model)

run.df3 = function()
{
  # Process data
  df3.processed = process.data(df3, model="CJS", groups= c("sex"))
  
  # Create default design data
  df3.ddl = make.design.data(df3.processed) 
  
  #  Define range of models for Phi (survival probability)
  Phi.dot = list(formula = ~ 1)
  Phi.sex = list(formula = ~ sex)
  Phi.weight = list(formula = ~ mweight)
  Phi.rhod = list(formula = ~ rhodo)
  #Phi.logs = list(formula = ~ logs)
  Phi.logstump = list(formula = ~ logstump)
  Phi.moon = list(formula = ~ moon)
  
  Phi.cov = list(formula = ~ cover)
  Phi.cov.moon = list(formula = ~ moon * cover)
  Phi.cov.mweight = list(formula = ~ cover * mweight)
  Phi.cov.spaceuse = list(formula = ~ cover * spaceuse)
  Phi.cov.sex = list(formula = ~ cover * sex)
  
  Phi.spaceuse = list(formula = ~ spaceuse)#
  Phi.spaceuse.rhodo = list(formula = ~ spaceuse * rhodo)#
  Phi.moon.spaceuse = list(formula = ~ moon * spaceuse)#
  #Phi.spaceuse.logs = list(formula = ~ spaceuse * logs)#
  Phi.spaceuse.logstump = list(formula = ~ spaceuse * logstump)#
  Phi.mweight.spaceuse = list(formula = ~ spaceuse * mweight)#
  
  Phi.sex.weight = list(formula = ~ sex * mweight)
  Phi.sex.rhod = list(formula = ~ sex * rhodo)
  #Phi.sex.logs = list(formula = ~ sex * logs)
  Phi.sex.logstump = list(formula = ~ sex * logstump)
  Phi.mweight.rhod = list(formula = ~ mweight * rhodo)
  #Phi.mweight.logs = list(formula = ~ mweight * logs)
  Phi.mweight.logstump = list(formula = ~ mweight * logstump)
  Phi.moon.rhod = list(formula = ~ moon * rhodo)
  Phi.moon.logstump = list(formula = ~ moon * logstump)
  
  #  Define range of models for p (capture probability)
  p.dot = list(formula = ~ 1)

  # Print statement
  cat("Running all the models will take around 1 hour!!!!!")
  
  # Run assortment of models
  mod.list = create.model.list("CJS")
  results = mark.wrapper(mod.list, data = df3.processed, ddl = df3.ddl)
  
  # Return model table and list of models
  return(results )
}

# Run models
results1.wrapper = run.df3()

################ Extract results from the top models ################

########## Rate of activity X Rhododendron

# Create a list of values for model predictions
meanspace <- mean(df3$spaceuse)
spaceuseval <- seq(from = min(df3$spaceuse), to = max(df3$spaceuse), length.out = 100)
meanrhod <- mean(df3$rhodo)
rhodval <- seq(from = min(df3$rhodo), to = max(df3$rhodo), length.out = 100)

# Phi estimations with standard error
phi=coef(results1.wrapper[[21]])[c(1,4),]
plogis(phi$estimate[1]+(rhodval*meanspace)*phi$estimate[2])

logit.values=phi$estimate[1]+(rhodval*meanspace)*phi$estimate[2]
deriv=matrix(1,ncol=2,nrow=100)
deriv[,2]=(rhodval*meanspace)

std.errors=sqrt(diag(deriv%*%results1.wrapper[[21]]$results$beta.vcv[c(1,4),c(1,2)]%*%t(deriv)))
std.errors[is.nan(std.errors)] <- 0
lcl.logit=logit.values-1.96*std.errors
ucl.logit=logit.values+1.96*std.errors

# Simple graph plot
plot((rhodval*meanspace),plogis(logit.values),type="l",xlab = "Rate of activity (metres/seconds)",
     ylab = "Daily survival probability")
lines((rhodval*meanspace),plogis(lcl.logit),lty=2)
lines((rhodval*meanspace),plogis(ucl.logit),lty=2)

# Create data frame of predicted results
hi3 <- data.frame(spaceval = (rhodval*meanspace), logit.values = plogis(logit.values),
                  high = plogis(ucl.logit), low = plogis(lcl.logit))

# Draft ggplot graph 
ggplot(hi3, aes(x = spaceval, y = logit.values)) +
  geom_line(col = 'red', size = 0.8) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1) +
  xlab("Rate of activity (metres per second)") +
  ylab("Daily survival estimate") +
  #coord_cartesian(ylim = c(0.85,1)) +
  ggtitle("Effects of activity rate on rodent's survival") +
  theme_bw() +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

# Save in the Results directory
write.csv(hi3, "../Results/spaceuseRhodSUR.csv")

########## Rhodo from Rhodo X Weight

# Create a list of values for model predictions
rhodval <- seq(from = min(df3$rhodo), to = max(df3$rhodo), length.out = 100)

# Phi estimations with standard error
phi=coef(results1.wrapper[[13]])[c(1,3),]
plogis(phi$estimate[1]+(rhodval)*phi$estimate[2])

logit.values=phi$estimate[1]+(rhodval)*phi$estimate[2]
deriv=matrix(1,ncol=2,nrow=100)
deriv[,2]=(rhodval)

std.errors=sqrt(diag(deriv%*%results1.wrapper[[13]]$results$beta.vcv[c(1,3),c(1,2)]%*%t(deriv)))
std.errors[is.nan(std.errors)] <- 0
lcl.logit=logit.values-1.96*std.errors
ucl.logit=logit.values+1.96*std.errors

# Simple graph plot
plot((rhodval),plogis(logit.values),type="l",xlab = "Rate of activity (metres/seconds)",
     ylab = "Daily survival probability")
lines((rhodval),plogis(lcl.logit),lty=2)
lines((rhodval),plogis(ucl.logit),lty=2)

# Create data frame of predicted results
hi4 <- data.frame(spaceval = (rhodval), logit.values = plogis(logit.values),
                  high = plogis(ucl.logit), low = plogis(lcl.logit))

# Draft ggplot graph
ggplot(hi4, aes(x = spaceval, y = logit.values)) +
  geom_line(col = 'red', size = 0.8) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1) +
  xlab("Rhododendron cover (%)") +
  ylab("Daily survival estimate") +
  #coord_cartesian(ylim = c(0.85,1)) +
  ggtitle("Effects of Rhododendron on rodent's survival") +
  theme_bw() +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

# Save in the Results directory
write.csv(hi4, "../Results/rhodrhodwtSUR.csv")

########## Rate of activity

# Testing...
# fc <- find.covariates(results1.wrapper[[10]], df3)
# fc$value[fc$var=="rhodo"]=0
# design=fill.covariates(results1.wrapper[[10]],fc)

# Create a list of values for model predictions
spaceuseval <- seq(from = min(df3$spaceuse), to = max(df3$spaceuse), length.out = 100)

# Phi estimations with standard error
phi=coef(results1.wrapper[[11]])[c(1,3),]
plogis(phi$estimate[1]+spaceuseval*phi$estimate[2])

logit.values=phi$estimate[1]+spaceuseval*phi$estimate[2]
deriv=matrix(1,ncol=2,nrow=100)
deriv[,2]=spaceuseval

std.errors=sqrt(diag(deriv%*%results1.wrapper[[11]]$results$beta.vcv[c(1,3),c(1,2)]%*%t(deriv)))
std.errors[is.nan(std.errors)] <- 0
lcl.logit=logit.values-1.94*std.errors
ucl.logit=logit.values+1.94*std.errors

# Simple graph plot
plot(spaceuseval,plogis(logit.values),type="l",xlab = "Rate of activity (metres/seconds)",
     ylab = "Daily survival probability")
lines(spaceuseval,plogis(lcl.logit),lty=2)
lines(spaceuseval,plogis(ucl.logit),lty=2)

# Create data frame of predicted results
hi2 <- data.frame(spaceval = spaceuseval, logit.values = plogis(logit.values),
                 high = plogis(ucl.logit), low = plogis(lcl.logit))

# Draft ggplot graph
ggplot(hi2, aes(x = spaceval, y = logit.values)) +
  geom_line(col = 'red', size = 0.8) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1) +
  xlab("Rate of activity (metres per second)") +
  ylab("Daily survival estimate") +
  #coord_cartesian(ylim = c(0.85,1)) +
  ggtitle("Effects of activity rate on rodent's survival") +
  theme_bw() +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

# Save in the Results directory
write.csv(hi2, "../Results/spaceuseSUR.csv")

######################### Rhododendron only

# Create a list of values for model predictions
rhodval <- seq(from = min(df3$rhodo), to = max(df3$rhodo), length.out = 100)

# Phi estimations with standard error
phi=coef(results1.wrapper[[21]])[c(1,3),]
plogis(phi$estimate[1]+rhodval*phi$estimate[2])

logit.values=phi$estimate[1]+rhodval*phi$estimate[2]
deriv=matrix(1,ncol=2,nrow=100)
deriv[,2]=rhodval

std.errors=sqrt(diag(deriv%*%results1.wrapper[[21]]$results$beta.vcv[c(1,3),c(1,2)]%*%t(deriv)))
lcl.logit=logit.values-1.96*std.errors
ucl.logit=logit.values+1.96*std.errors

# Simple graph plot
plot(rhodval,plogis(logit.values),type="l",xlab = "Rate of activity (metres/seconds)",
     ylab = "Daily survival probability")
lines(rhodval,plogis(lcl.logit),lty=2)
lines(rhodval,plogis(ucl.logit),lty=2)

# Create data frame of predicted results
rhodonly <- data.frame(rhodval = rhodval, logit.values = plogis(logit.values), 
                 high = plogis(ucl.logit), low = plogis(lcl.logit))

# Draft ggplot graph
ggplot(rhodonly, aes(x = rhodval, y = logit.values)) +
  geom_line(col = 'red', size = 1) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1) +
  ylim(0.97,0.99) +
  theme_bw() +
  xlab("Rhododendron cover (%)") +
  ylab("Daily survival estimate") +
  ggtitle("Effects of Rhododendron on rodent's survival") +
  #coord_cartesian(ylim = c(0.85,1)) +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

# Save in the Results directory
write.csv(rhodonly, "../Results/rhodSUR.csv")

######################### Rhododendron with weight

# Create a list of values for model predictions
rhodval <- seq(from = min(df3$rhodo), to = max(df3$rhodo), length.out = 100)
mweight <- seq(from = min(df3$mweight), to = max(df3$mweight), length.out = 100)

# Phi estimations with standard error
phi=coef(results1.wrapper[[13]])[c(1,4),]
plogis(phi$estimate[1]+(mweight*rhodval)*phi$estimate[2])

logit.values=phi$estimate[1]+(mweight*rhodval)*phi$estimate[2]
deriv=matrix(1,ncol=2,nrow=100)
deriv[,2]=(mweight*rhodval)

std.errors=sqrt(diag(deriv%*%results1.wrapper[[13]]$results$beta.vcv[c(1,4),c(1,2)]%*%t(deriv)))
std.errors[is.nan(std.errors)] <- 0
lcl.logit=logit.values-1.96*std.errors
ucl.logit=logit.values+1.96*std.errors

# Simple graph plot
plot((mweight*rhodval),plogis(logit.values),type="l",xlab = "Rate of activity (metres/seconds)",
     ylab = "Daily survival probability")
lines((mweight*rhodval),plogis(lcl.logit),lty=2)
lines((mweight*rhodval),plogis(ucl.logit),lty=2)

# Create data frame of predicted results
rhodwt <- data.frame(rhodweight = (mweight*rhodval), logit.values = plogis(logit.values), 
                       high = plogis(ucl.logit), low = plogis(lcl.logit))

# Draft ggplot graph
ggplot(rhodwt, aes(x = rhodweight, y = logit.values)) +
  geom_line(col = 'red', size = 1) +
  geom_ribbon(aes(x = rhodweight, ymin = low, ymax = high), alpha = 0.1) +
  theme_bw() +
  xlab("Mouse weight (grams) * Rhododendron cover (%)") +
  ylab("Daily survival estimate") +
  coord_cartesian(ylim = c(0.85,1)) +
  #ggtitle("Effects of activity rate on rodent's survival") +
  
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

# Save in the Results directory
write.csv(rhodwt, "../Results/rhodwtSUR.csv")

############################ Spaceuse with moon

# Create a list of values for model predictions
meanmoon <- 48.65399 # mean of non duplicated date columns from space3
moonlight <- seq(from = min(df3$moon), to = max(df3$moon), length.out = 50)
spaceuseval <- seq(from = min(df3$spaceuse), to = max(df3$spaceuse), length.out = 100)

# moonspace <- expand.grid(spaceuseval, moonlight)
# moonspace$var3 <- moonspace$Var1 * moonspace$Var2

# Phi estimations with standard error
phi=coef(results1.wrapper[[11]])[c(1,4),]
plogis(phi$estimate[1]+(spaceuseval * meanmoon)*phi$estimate[2])

logit.values=phi$estimate[1]+(spaceuseval * meanmoon)*phi$estimate[2]
deriv=matrix(1,ncol=2,nrow=100)
deriv[,2]=(spaceuseval * meanmoon)

std.errors=sqrt(diag(deriv%*%results1.wrapper[[11]]$results$beta.vcv[c(1,4),c(1,2)]%*%t(deriv)))
std.errors[is.nan(std.errors)] <- 0
lcl.logit=logit.values-1.96*std.errors
ucl.logit=logit.values+1.96*std.errors

# Simple graph plot
plot((spaceuseval * meanmoon),plogis(logit.values),type="l",xlab = "Rate of activity (metres/seconds)",
     ylab = "Daily survival probability")
lines((spaceuseval * meanmoon),plogis(lcl.logit),lty=2)
lines((spaceuseval * meanmoon),plogis(ucl.logit),lty=2)

# Create data frame of predicted results
moonspace <- data.frame(rhodweight = (spaceuseval * meanmoon), logit.values = plogis(logit.values), 
                     high = plogis(ucl.logit), low = plogis(lcl.logit))

# Draft ggplot graph
ggplot(moonspace, aes(x = rhodweight, y = logit.values)) +
  geom_line(col = 'red') +
  geom_ribbon(aes(x = rhodweight, ymin = low, ymax = high), alpha = 0.1) +
  theme_bw() +
  xlab("Activity rate under average moon light illumination \n(metres/minutes * 48.6)") +
  ylab("Daily survival estimate") +
  coord_cartesian(xlim = c(0,150)) +
  #ggtitle("Effects of activity rate on rodent's survival") +
  
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black')) 
  
  # # remove x axis numbers
  # theme(#axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),)
  #       #axis.ticks.x=element_blank())

# Save in the Results directory
write.csv(moonspace, "../Results/moonspaceSUR.csv")












######################## NOT NEEDED ########################

# AIC table of all models

# ### cp3
# results4.wrapper #wt first, 13 (wt rhod, 2.92), 2 (cv wt, 2.95), 12 (wt log, 3)
# 
# results3.wrapper #mnrhod first, 12 (wt log, 2.73), 13 (wt rhod, 95%), 23 (wt, 95%)
# 
# results2.wrapper #mnrhod first, 13 (wt rhod), 23 (wt), 2 (cov wt), 14 (sp wt), 12 (wt log)
# 
# results1.wrapper # 11 (spaceuse moon), 14 (spaceuse wt), 13 (wt rhod), 2 (cover wt, meh)
# 
# 
# ### cp4
# results1.wrapper #mnsp, 14 (sp wt), 13 (wt rhod)

# caphistory2
#                             model npar     AICc DeltaAICc       weight Deviance
# 8      Phi(~mweight * rhodo)p(~1)    5 36316.07   0.00000 7.895832e-01 36306.06
# 10               Phi(~rhodo)p(~1)    3 36320.61   4.53535 8.176342e-02 36314.60
# 9   Phi(~spaceuse * mweight)p(~1)    5 36321.29   5.21500 5.820698e-02 36311.28
# 5         Phi(~moon * rhodo)p(~1)    5 36321.78   5.71200 4.539969e-02 36311.78
# 13         Phi(~sex * rhodo)p(~1)    5 36323.68   7.61000 1.757549e-02 36313.67
# 18             Phi(~mweight)p(~1)    3 36327.10  11.03135 3.176659e-03 36321.10
# 7   Phi(~mweight * logstump)p(~1)    5 36328.90  12.83300 1.290468e-03 36318.90
# 16            Phi(~spaceuse)p(~1)    3 36329.47  13.39635 9.736825e-04 36323.46
# 14       Phi(~sex * mweight)p(~1)    5 36330.11  14.03900 7.061026e-04 36320.10
# 15 Phi(~spaceuse * logstump)p(~1)    5 36330.40  14.32900 6.107945e-04 36320.39
# 6      Phi(~moon * spaceuse)p(~1)    5 36332.77  16.70100 1.865614e-04 36322.76
# 1                    Phi(~1)p(~1)    2 36333.06  16.98980 1.614766e-04 35920.89
# 2             Phi(~logstump)p(~1)    3 36333.78  17.71135 1.125711e-04 36327.78
# 4                 Phi(~moon)p(~1)    3 36334.05  17.97735 9.855209e-05 36328.05
# 12                 Phi(~sex)p(~1)    3 36334.19  18.12035 9.175163e-05 35920.03
# 11      Phi(~sex * logstump)p(~1)    5 36335.73  19.65800 4.253216e-05 36325.72
# 3      Phi(~moon * logstump)p(~1)    5 36337.25  21.17700 1.990081e-05 36327.24
# 17    Phi(~spaceuse * rhodo)p(~1)    5 36346.75  30.68100 1.718317e-07 36336.75

# caphistoryCOMB2
#                             model npar     AICc DeltaAICc       weight Deviance
# 17    Phi(~spaceuse * rhodo)p(~1)    5 47053.62    0.0000 9.953082e-01 47043.61
# 5         Phi(~moon * rhodo)p(~1)    5 47065.17   11.5530 3.085002e-03 47055.17
# 8      Phi(~mweight * rhodo)p(~1)    5 47067.42   13.7970 1.004563e-03 47057.41
# 10               Phi(~rhodo)p(~1)    3 47070.26   16.6381 2.426837e-04 47064.25
# 9   Phi(~spaceuse * mweight)p(~1)    5 47070.33   16.7120 2.338799e-04 47060.32
# 13         Phi(~sex * rhodo)p(~1)    5 47072.29   18.6730 8.773384e-05 47062.29
# 7   Phi(~mweight * logstump)p(~1)    5 47076.18   22.5650 1.253228e-05 47066.18
# 16            Phi(~spaceuse)p(~1)    3 47076.29   22.6711 1.188478e-05 47070.29
# 15 Phi(~spaceuse * logstump)p(~1)    5 47077.26   23.6430 7.310471e-06 47067.26
# 18             Phi(~mweight)p(~1)    3 47078.07   24.4481 4.887880e-06 47072.07
# 14       Phi(~sex * mweight)p(~1)    5 47080.92   27.3010 1.173872e-06 47070.91
# 2             Phi(~logstump)p(~1)    3 47087.50   33.8831 4.368540e-08 47081.50
# 1                    Phi(~1)p(~1)    2 47087.70   34.0848 3.949461e-08 45932.83
# 12                 Phi(~sex)p(~1)    3 47088.76   35.1431 2.326649e-08 45931.89
# 4                 Phi(~moon)p(~1)    3 47089.01   35.3871 2.059429e-08 47083.00
# 3      Phi(~moon * logstump)p(~1)    5 47089.12   35.5050 1.941533e-08 47079.12
# 6      Phi(~moon * spaceuse)p(~1)    5 47089.60   35.9840 1.528028e-08 47079.60
# 11      Phi(~sex * logstump)p(~1)    5 47090.65   37.0260 9.075359e-09 47080.64

# Phi.mweight.rhod = list(formula = ~ mweight : rhodo)
# p.dot = list(formula = ~ 1)
# top <- mark(data = df3.processed, ddl = df3.ddl,
#             model.parameters=list(Phi=Phi.mweight.rhod,p=p.dot))
# top$results$beta
#                   estimate        se        lcl        ucl
# Phi:(Intercept)    4.5262079 0.6347198  3.2821571  5.7702587 # Constant survival porbability
# Phi:mweight        0.0082759 0.0338626 -0.0580947  0.0746465
# Phi:rhodo         -2.8698344 1.9160166 -6.6252270  0.8855582
# Phi:mweight:rhodo  0.1943819 0.1011133 -0.0038003  0.3925640
# p:(Intercept)     -1.1831718 0.0134609 -1.2095553 -1.1567884 # Constant capture probability

## Looking at sex difference only

# Phi.sex = list(formula = ~ sex)
# p.dot = list(formula = ~ 1)
# 
# modelsex <- mark(data = df3.processed, ddl = df3.ddl, model = "CJS",
#                  model.parameters=list(Phi = Phi.sex))

# ########## Rate of activity
# fc <- find.covariates(results1.wrapper[[10]], df3)
# fc$value[fc$var=="rhodo"]=0
# design=fill.covariates(results1.wrapper[[10]],fc)
# 
# spaceuseval <- seq(from = min(df3$spaceuse), to = max(df3$spaceuse), length.out = 100)
# 
# phi=coef(results1.wrapper[[14]])[1:2,]
# plogis(phi$estimate[1]+spaceuseval*phi$estimate[2])
# 
# logit.values=phi$estimate[1]+spaceuseval*phi$estimate[2]
# deriv=matrix(1,ncol=2,nrow=100)
# deriv[,2]=spaceuseval
# 
# std.errors=sqrt(diag(deriv%*%results1.wrapper[[14]]$results$beta.vcv[1:2,c(1,2)]%*%t(deriv)))
# lcl.logit=logit.values-1.94*std.errors
# ucl.logit=logit.values+1.94*std.errors
# 
# plot(spaceuseval,plogis(logit.values),type="l",xlab = "Rate of activity (metres/seconds)",
#      ylab = "Daily survival probability")
# lines(spaceuseval,plogis(lcl.logit),lty=2)
# lines(spaceuseval,plogis(ucl.logit),lty=2)
# 
# hi <- data.frame(spaceval = spaceuseval, logit.values = plogis(logit.values),
#                  high = plogis(ucl.logit), low = plogis(lcl.logit))
# 
# ggplot(hi, aes(x = spaceval, y = logit.values)) +
#   geom_line(col = 'red') +
#   geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1) +
#   xlab("Rate of activity (metres per second)") +
#   ylab("Daily survival estimate") +
#   #coord_cartesian(ylim = c(0.85,1)) +
#   ggtitle("Effects of activity rate on rodent's survival") +
#   theme_bw() +
#   #eliminates background, gridlines, and chart border
#   theme(
#     plot.background = element_blank(),
#     panel.grid.major = element_blank(),
#     panel.grid.minor = element_blank(),
#     panel.border = element_blank()
#   ) +
# 
#   #draws x and y axis line
#   theme(axis.line = element_line(color = 'black'))
# 
# write.csv(hi, "../../../Downloads/spaceuseSUR.csv")

# Change activity rate graph x axis, add rhodo X activity rate

# IF MODEL OVER-PARAMETERIZED
# SE MIGHT BE MASSIVE!! 1700..
# LOOK AT DM, (MOD0$DESIGN.MATRIX)
# EACH ROW SHOULD ONLY HAVE ONE 1

# Plot survival prediction with rhododendron
# Phi.rhod = list(formula = ~ rhodo)
# p.dot = list(formula = ~ 1)
# modelrhod <- mark(data = df3.processed, ddl = df3.ddl, model = "CJS",
#             model.parameters=list(Phi = Phi.rhod))
# 
# results2.wrapper[[10]]$results$beta
# results2.wrapper[[10]]$results$real # can check SE
# 
# # phitable = get.real(results2.wrapper[[10]],"Phi", se= TRUE)
# # # names(phitable)
# # phitable[c("estimate","se","lcl","ucl")][1,]
# 
# summary(results1.wrapper[[10]],se=TRUE)$real
# PIMS(results1.wrapper[[10]], "Phi", simplified = F)
# 
# rhod.values <- seq(from = min(df3$rhodo), to = max(df3$rhodo), length.out = 100)
# 
# phi.rhod.predictions = covariate.predictions(results1.wrapper[[10]],
#                                              data = data.frame(rhod=rhod.values), 
#                                              indices = rep(1,50)) #,63191,63191,63546,63546
# 
# phi.rhod.predictions$estimates
# 
# plot(phi.rhod.predictions$estimates$covdata, phi.rhod.predictions$estimates$estimate)
# 
# phi.rhod.predictions = covariate.predictions(results2.wrapper[[10]],
#                                              data = data.frame(rhod=rhod.values), 
#                                              rhod = seq(from = min(df3$rhodo), 
#                                                         to = max(df3$rhodo), 
#                                                         length.out = 50))
# 
# 
# phi.rhod.predictions = covariate.predictions(model = modelrhod,
#                                              data = data.frame(rhod.values),
#                                              indices = c(min(which(df3.ddl$Phi$group=="M")), 
#                                                          min(which(df3.ddl$Phi$group=="F"))))
# 
# phi.rhod.predictions = covariate.predictions(model = modelrhod,
#                                              data = data.frame(index = c(1, 62837),
#                                                                rhod.values))
# 
# with(phi.rhod.predictions$estimates, 
#      { 
#        plot(rhod.values, estimate,type="l",lwd=2,xlab="Rhododendron", 
#             ylab="Survival",ylim=c(0,1)) 
#        lines(rhod.values,lcl,lty=2) 
#        lines(rhod.values,ucl,lty=2) 
#      })
# 
# #
# Phi.mweight = list(formula = ~ mweight)
# model.mweight <- mark(data = df3.processed, ddl = df3.ddl,
#                       model.parameters=list(Phi = Phi.mweight, p = p.dot))
# model.mweight$results$beta
# 
# phi.mweight.predictions = covariate.predictions(model = model.mweight,
#                                              data = data.frame(index=rep(1:50), 
#                                                                rhod = seq(from = min(df3$mweight), 
#                                                                           to = max(df3$mweight), 
#                                                                           length.out = 50)))
# with(phi.mweight.predictions$estimates, 
#      { 
#        plot(mass, estimate,type="l",lwd=2,xlab="Rhododendron", 
#             ylab="Survival",ylim=c(0,1)) 
#        lines(mass,lcl,lty=2) 
#        lines(mass,ucl,lty=2) 
#      })
# 
# # Spaceuse * rhodo
# PIMS(results2.wrapper[[17]], "Phi", simplified = F)
# Phibyspacerhod = covariate.predictions(results2.wrapper[[17]],
#                                        data = )