################ Generalised linear mixed effects models ################

# Load libraries
library(lme4)
library(lmerTest) 
#library(car) # qqp() quantile-comparison plot
library(performance) # check VIF
library(glmmTMB) # negative binomial glmm
library(MuMIn) # AICc


# https://cran.r-project.org/web/packages/DHARMa/vignettes/DHARMa.html
library(DHARMa) # test for dispersion

# Read data
# Per night per individual
df <- read.csv("../Data/pernightperID7update.csv", 
               colClasses = c(Sex = "factor", season = "factor", ID = "factor", season.n = "factor"))

# Remove males
df <- subset(df, Sex == "F")

# Subset out individuals that only visited one data logger
df1 <- subset(df, Diff_logger != 1)

# Convert moon illumination (%) to 0-1
df$Moon2 <- df$Moon/100
df1$Moon2 <- df1$Moon/100

# Show that data has a crossed structure, multiple IDs in several season.n
# with(df, table(ID, season.n)) %>%
#      image(
#          col = grey.colors(10, start = 1, end = 0), 
#          axes = FALSE, 
#          xlab = "ID", 
#          ylab = "Season.n"
#      )

cat("Running all models will take hours, sit back and take a break !!!!!!!!!")
cat("Alternative: only run the optimal model for each response variable !!")

################################################################################################

# 5 response variables: total hits, diff-loggers, distance20, act_time, activity20
# logged - df1$act-time(right-skewed), df1$distance20(normal), df1$activity20(normal)
# Count data - total hits, diff_loggers (Poisson or quasipoisson)
# 

# Predictors - rhododendron, logs, season, sex, moon

# Problems : 2 errors in glmer, one potentially links to collnearity in fixed effects, how to fix?

# Check which model distribution with the appropirate link function fits the best 
# Determine by AIC values

try(glmm.a <- glmmTMB(Total_hits ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                      data = df, family="nbinom1"))

try(glmm.b <- glmmTMB(Total_hits ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                      data = df, family="nbinom2"))

try(glmm.c <- glmer(Total_hits ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                    data = df, family=poisson(link="log"),
                    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))))

try(glmm.d <- glmer(Total_hits ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                    data = df, family=poisson(link="identity"),
                    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))))

try(glmm.e <- glmer(Total_hits ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                    data = df, family=poisson(link="sqrt"),
                    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))))

try(glmm.f <- glmer(Total_hits ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                    data = df, family=Gamma(link="identity"),
                    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))))

try(glmm.g <- glmer(Total_hits ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                    data = df, family=Gamma(link="log"),
                    glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))))

# AIC score compare, lowest are: g&f (gamma), followed by c (poisson) and b (nbinom)
AICc(glmm.a,glmm.b,glmm.c,glmm.d,glmm.e,glmm.f,glmm.g)

# Test for dispersion on the best model based on AIC values
testDispersion(glmm.b) # shows g is under dispersed, so we will use model c/b

# Check collinearity between variables
check_collinearity(glmm.b) # All lower than 5

# #
# qqnorm(resid(glmm.b))
# abline(0, 1, col = 'red')
# plot(resid(glmm.b))
# plot(glmm.b)
# hist(resid(glmm.b), breaks = 50)

# QQ plot residuals and residual vs predicted
simulationOutput <- simulateResiduals(fittedModel = glmm.b)
plot(simulationOutput)

# #Is the data approximately distributed appropriately (negative binomial)? 
# poisson <- fitdistr(df$Total_hits, "Gamma")
# qqp(df$Total_hits, "Gamma", lambda=poisson$estimate, main="Gamma model")
# 
# 
# nbinom<-fitdistr(df$Total_hits, "negative binomial")
# qqp(df$Total_hits, "nbinom", size=nbinom$estimate[[1]], 
#     mu=nbinom$estimate[[2]], main="Negative binomial model")

# anova from lmerTest for Satterwaithe approximation, p-values tho

# Model reduction
drop.scope(glmm.b)

glmm.b.2 <- update(glmm.b, .~. - Logstump:Moon2)

glmm.b.3 <- update(glmm.b.2, .~. - datalog)

AICc(glmm.b.3, glmm.b.2, glmm.b) # b.2 best AICc

# Conditional model:
#   Groups   Name        Variance Std.Dev.
# ID       (Intercept) 0.06424  0.2534  
# season.n (Intercept) 0.05288  0.2300  
# Number of obs: 3604, groups:  ID, 166; season.n, 9
# 
# Dispersion parameter for nbinom2 family ():   86 
# 
#   Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)    -0.111024   0.261946  -0.424  0.67168    
#   datalog         0.042152   0.021018   2.005  0.04491 *  
#   trap            0.635170   0.217656   2.918  0.00352 ** 
#   Rhodo          -0.217716   0.052463  -4.150 3.33e-05 ***
#   Logstump       -0.007287   0.009339  -0.780  0.43526    
#   Moon2          -0.116679   0.045501  -2.564  0.01034 *  
#   Rhodo:Logstump  0.127375   0.027930   4.561 5.10e-06 ***
#   Rhodo:Moon2     0.217102   0.075794   2.864  0.00418 ** 

# R2
# Marginal R2: fixed effect variance/total variance (fixed,random,residual)
# indicates how much of the model variance is explained by the fixed effects only
# Conditional R2: fixed + random effect variances/total variance
# indicates how much of the model variance is explained by the complete model
# ICC: how much variance is explained by random effects only

performance::r2(glmm.b.2) # Conditional: 0.227, Marginal: 0.023
performance::icc(glmm.b.2) # Adjusted ICC: 0.209, Conditional ICC: 0.204

################################################################################################

# Find the nu (dispersion) parameter for Conway-Maxwell-Poisson mixed model fitting
# system.time(
#   fitme(Diff_logger ~ 1 + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
#         data = df, family = COMPoisson())
# )


## GLM fitting
try(glmm.dl.a <- glmmTMB(Diff_logger ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                         data = df, family="genpois"))

try(glmm.dl.b <- glmmTMB(Diff_logger ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                          data = df, family="compois"))

try(glmm.dl.c <- glmer(Diff_logger ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                       data = df, family=poisson(link="log"),
                       glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))))

try(glmm.dl.d <- glmer(Diff_logger ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                       data = df, family=poisson(link="identity"),
                       glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))))

try(glmm.dl.e <- glmer(Diff_logger ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                       data = df, family=poisson(link="sqrt"),
                       glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))))

try(glmm.dl.f <- glmer(Diff_logger ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                       data = df, family=Gamma(link="identity"),
                       glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))))

try(glmm.dl.g <- glmer(Diff_logger ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                       data = df, family=Gamma(link="log"),
                       glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000))))

system.time(
  try(glmm.dl.h <- glmmTMB(Diff_logger ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                           data = df, family = nbinom1))
)

system.time(
  try(glmm.dl.hh <- glmmTMB(Diff_logger ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                            data = df, family = nbinom2))
)

# AIC to compare best scores: 
AICc(glmm.dl.a,glmm.dl.c,glmm.dl.d,glmm.dl.e,
    glmm.dl.f,glmm.dl.g,glmm.dl.h,glmm.dl.hh) # gamma best, followed by a (genpois)

# Test for dispersion on the best model based on AIC values
testDispersion(glmm.dl.a) 
# gamma: over dispersion, 1.5517, COMp: over dispersion, 1.38
# Gen-pois: no under/over-dispersion!!

# Check collinearity between variables
check_collinearity(glmm.dl.a) # All VIF lower than 5

# hist(resid(glmm.dl.b), breaks = 50) # Residual distribution
# qqnorm(resid(glmm.dl.b))
# abline(0, 1, col = 'red')
# plot(fitted(glmm.dl.b),resid(glmm.dl.b)) # linearity


# QQ plot residuals and residual vs predicted
simulationOutput.dl <- simulateResiduals(fittedModel = glmm.dl.a)
plot(simulationOutput.dl)

# #Deviance GOF
# pchisq(glmm.dl.g$deviance/summary(glmm.dl.g$disp), df = glmm.dl.g$df.residual, lower.tail = F)

### Model reduction
drop.scope(glmm.dl.a)

glmm.dl.a.2 <- update(glmm.dl.a, .~. - Logstump:Moon2) # dropping the non-significant interaction term
glmm.dl.a.3 <- update(glmm.dl.a.2, .~. - trap - datalog)

AICc(glmm.dl.a, glmm.dl.a.2, glmm.dl.a.3) # a.2 with best AICc

#             df      AIC
# glmm.dl.a   12 8133.273
# glmm.dl.a.2 11 8131.319
# glmm.dl.a.3  9 8141.982

# Random effects:
#   
#   Conditional model:
#   Groups   Name        Variance Std.Dev.
# ID       (Intercept) 0.11723  0.3424  
# season.n (Intercept) 0.04895  0.2212  
# Number of obs: 3604, groups:  ID, 166; season.n, 9
# 
# Dispersion parameter for genpois family (): 0.481 
# 
#   Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)    -0.458416   0.209745  -2.186 0.028845 *  
#   datalog         0.056671   0.016510   3.433 0.000598 ***
#   trap            0.277547   0.158222   1.754 0.079403 .  
#   Rhodo          -0.231823   0.043336  -5.349 8.83e-08 ***
#   Logstump       -0.022834   0.007572  -3.016 0.002565 ** 
#   Moon2          -0.125662   0.035456  -3.544 0.000394 ***
#   Rhodo:Logstump  0.205656   0.023206   8.862  < 2e-16 ***
#   Rhodo:Moon2     0.204592   0.060373   3.389 0.000702 ***

# R2
# Marginal R2: fixed effect variance/total variance (fixed,random,residual)
# indicates how much of the model variance is explained by the fixed effects only
# Conditional R2: fixed + random effect variances/total variance
# indicates how much of the model variance is explained by the complete model
# ICC: how much variance is explained by random effects only

performance::r2(glmm.dl.a.2) # Conditional: 0.346, Marginal: 0.023
performance::icc(glmm.dl.a.2) # Adjusted ICC: 0.352, Conditional ICC: 0.347


################################################################################################
# Continuous non-negative: Gamma and inverse gaussian distributions
# hist(df1$Distance20, breaks = 50) # Both eft-skewed
# hist(df1$Act_time, breaks = 50)

# Rescale and center response variables - Distance20
# GLMM warnings: convergence failure due to very large eigenvalue, hence rescaling

# # rescaling for both data sets - df & df1
# numcolss <- grep("Act_t",names(df))
# numcolss1 <- grep("Distance20",names(df))
# dfs <- df
# dfs[,numcolss] <- scale(dfs[,numcols])
# dfs[,numcolss1] <- scale(dfs[,numcols1])
# 
# numcols <- grep("Act_t",names(df1))
# numcols1 <- grep("Distance20",names(df1))
# df1s <- df1
# df1s[,numcols] <- scale(df1s[,numcols])
# df1s[,numcols1] <- scale(df1s[,numcols1])
# 
# # Normal distribution for both response variables
# hist(df1s$Act_time, breaks = 50)
# hist(df1s$Distance20, breaks = 50)

################################################################################################

# LMM for Distance
lmm.dis.a <- lmer(log(Distance20) ~ 1 + datalog+trap+ Rhodo * Moon * Logstump + (1|ID) + (1|season.n), 
                  REML = F,
                  data = df1, control=lmerControl(optimizer="bobyqa"))

lmm.dis.b <- lmer(log(Distance20) ~ 1 + datalog+trap+ Rhodo * Moon * Logstump + (1|ID), 
                  REML = F,
                  data = df1, control=lmerControl(optimizer="bobyqa"))

lmm.dis.c <- lmer(log(Distance20) ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                  REML = F,
                  data = df1, control=lmerControl(optimizer="bobyqa"))

lmm.dis.a0 <- lmer(Distance20 ~ 1 + datalog+trap+ Rhodo * Moon * Logstump + (1|ID) + (1|season.n), 
                   REML = F,
                   data = df, control=lmerControl(optimizer="bobyqa"))

lmm.dis.b0 <- lmer(Distance20 ~ 1 + datalog+trap+ Rhodo * Moon * Logstump + (1|ID), 
                   REML = F,
                   data = df, control=lmerControl(optimizer="bobyqa"))

lmm.dis.c0 <- lmer(Distance20 ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                   REML = F,
                   data = df, control=lmerControl(optimizer="bobyqa"))

AICc(lmm.dis.a, lmm.dis.b, lmm.dis.c) # c best
AICc(lmm.dis.a0, lmm.dis.b0, lmm.dis.c0) # c0 best

summary(lmm.dis.c)
# anova(lmm.dis.c,lmm.dis.a)

## Check lmm assumptions
# Linearity
plot(resid(lmm.dis.c),log(df1$Distance20))

# For this portion of the analysis, we need to revisit about statistical significance 
# - since the assumption is that the variance is not going to differ, 
# we would hope to see NO STATISTICAL DIFFERENCES in the following procedure (i.e. p>0.05) to confirm that -

df1$Model.F.Res<- residuals(lmm.dis.c) #extracts the residuals and places them in a new column in our original data table
df1$Model.F.Res <-abs(df1$Model.F.Res) #creates a new column with the absolute value of the residuals
df1$Model.F.Res <- df1$Model.F.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.Model.F <- lm(Model.F.Res ~ ID, data=df1) #ANOVA of the squared residuals
anova(Levene.Model.F) # Non-significant!!

#
# qqmath(lmm.dis.c, id=0.05)

### Model reduction
drop.scope(lmm.dis.c)

lmm.dis.c.2 <- update(lmm.dis.c, .~. - Logstump:Moon2) # dropping the non-significant interaction term

AICc(lmm.dis.c, lmm.dis.c.2) #c.2 better

# df      AIC
# lmm.dis.c   10 7208.310
# lmm.dis.c.2  9 7206.379

# R2
performance::r2(lmm.dis.c.2) # Conditional: 0.249, Marginal: 0.021
performance::r2(lmm.dis.c) # Conditional: 0.249, Marginal: 0.021

performance::icc(lmm.dis.c.2) # Adjusted ICC: 0.233, Conditional ICC: 0.228
performance::icc(lmm.dis.c) # Adjusted ICC: 0.233, Conditional ICC: 0.228

## GLMM for df, accounting for all zeros!

try(glmm.dis.a <- glmmTMB(Distance20 ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                          data = df, ziformula = ~., family=truncated_nbinom1(link = "log")))

try(glmm.dis.b <- glmmTMB(Distance20 ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                          data = df, ziformula = ~., family=truncated_nbinom2(link = "log")))

try(glmm.dis.c <- glmmTMB(Distance20 ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                          data = df, ziformula = ~., family=truncated_poisson(link = "log")))

try(glmm.dis.d <- glmmTMB(Distance20 ~ 1 + datalog + trap + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                          data = df, ziformula = ~., family=ziGamma(link = "log")))

# try(glmm.dis.d <- glmmTMB(Distance20 ~ 1 + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
#                           data = df, ziformula = ~., family=tweedie(link = "log")))

AICc(glmm.dis.a, glmm.dis.b, glmm.dis.c, glmm.dis.d) # d best, followed by b

## Check GLM assumptions

# Test for dispersion on the best model based on AIC values
testDispersion(glmm.dis.d) 
# Gen-pois: no under/over-dispersion!!

# Check collinearity between variables
check_collinearity(glmm.dis.d)

# QQ plot residuals and residual vs predicted
simulationOutput.dis <- simulateResiduals(fittedModel = glmm.dis.d)
plot(simulationOutput.dis)

### Model reduction for c and c0
drop.scope(glmm.dis.d)

glmm.dis.d.2 <- update(glmm.dis.d, .~. - Logstump:Moon2) # dropping the non-significant interaction term
glmm.dis.d.3 <- update(glmm.dis.d.2, .~. - trap)
glmm.dis.d.4 <- update(glmm.dis.d.3, .~. - datalog)

#              df      AIC
# glmm.dis.d   23 21351.77
# glmm.dis.d.2 21 21351.21
# glmm.dis.d.3 19 21349.25
# glmm.dis.d.4 17 21351.93

AICc(glmm.dis.d, glmm.dis.d.2, glmm.dis.d.3, glmm.dis.d.4) # 3 best, need to retain datalog!


# Conditional model:
#   Groups   Name        Variance Std.Dev.
# ID       (Intercept) 0.12111  0.348   
# season.n (Intercept) 0.09485  0.308   
# Number of obs: 3604, groups:  ID, 166; season.n, 9
# 
# Zero-inflation model:
#   Groups   Name        Variance Std.Dev.
# ID       (Intercept) 0.4992   0.7065  
# season.n (Intercept) 0.1313   0.3624  
# Number of obs: 3604, groups:  ID, 166; season.n, 9
# 
# Dispersion estimate for Gamma family (sigma^2): 0.461 
# 
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)     3.409265   0.346912   9.827  < 2e-16 ***
#   datalog         0.072668   0.033373   2.177 0.029448 *  
#   Rhodo          -0.223569   0.086641  -2.580 0.009868 ** 
#   Logstump        0.007312   0.015258   0.479 0.631762    
#   Moon2          -0.226844   0.068698  -3.302 0.000960 ***
#   Rhodo:Logstump  0.222339   0.058318   3.813 0.000138 ***
#   Rhodo:Moon2     0.255470   0.121853   2.097 0.036034 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Zero-inflation model:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     1.12981    0.61034   1.851  0.06415 .  
# datalog        -0.08338    0.05978  -1.395  0.16303    
# Rhodo           0.49866    0.15404   3.237  0.00121 ** 
# Logstump        0.02750    0.02785   0.987  0.32347    
# Moon2           0.15007    0.13358   1.123  0.26125    
# Rhodo:Logstump -0.49479    0.10717  -4.617 3.89e-06 ***
# Rhodo:Moon2    -0.37149    0.22052  -1.685  0.09207 . 

# R2
performance::r2(glmm.dis.d.3) # Conditional: 0.339, Marginal: 0.029
performance::icc(glmm.dis.d.3) # Adjusted ICC: 0.319, Conditional ICC: 0.310


################################################################################################

# LMM for activity time, using two data sets: df and df1 (without zeros)
lmm.act.a <- lmer(log(Act_time) ~ 1 + datalog + trap + Rhodo * Moon * Logstump + (1|ID) + (1|season.n), 
                  REML = F,
                  data = df1, control=lmerControl(optimizer="bobyqa"))

lmm.act.b <- lmer(log(Act_time) ~ 1 + datalog + trap + Rhodo * Moon * Logstump + (1|ID), 
                  REML = F,
                  data = df1, control=lmerControl(optimizer="bobyqa"))

lmm.act.c <- lmer(log(Act_time) ~ 1 + datalog + trap + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                  REML = F,
                  data = df1, control=lmerControl(optimizer="bobyqa"))

lmm.act.a0 <- lmer(Act_time ~ 1 + datalog + trap + Rhodo * Moon * Logstump + (1|ID) + (1|season.n), 
                   REML = F,
                   data = df, control=lmerControl(optimizer="bobyqa"))

lmm.act.b0 <- lmer(Act_time ~ 1 + datalog + trap + Rhodo * Moon * Logstump + (1|ID), 
                   REML = F,
                   data = df, control=lmerControl(optimizer="bobyqa"))

lmm.act.c0 <- lmer(Act_time ~ 1 + datalog + trap + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                   REML = F,
                   data = df, control=lmerControl(optimizer="bobyqa"))

AIC(lmm.act.a, lmm.act.b, lmm.act.c) # a best
AIC(lmm.act.a0, lmm.act.b0, lmm.act.c0) # c best

summary(lmm.act.c0)
summary(lmm.act.c)
anova(lmm.act.c,lmm.act.a)

## Check lmm assumptions
# Linearity
plot(resid(lmm.act.c0),df$Act_time)

# We need to revisit statistical significance 
# since the assumption is that the variance is not going to differ, 
# we would hope to see NO STATISTICAL DIFFERENCES in the following procedure 
# Extract residual -> make them positive -> square -> anova squared residuals
df$Model.F.Res<- residuals(lmm.act.c0) 
df$Model.F.Res <-abs(df$Model.F.Res) 
df$Model.F.Res <- df$Model.F.Res^2 
Levene.Model.F <- lm(Model.F.Res ~ ID, data=df) 
anova(Levene.Model.F) # Non-significant for df, p:0.5!

#
# qqmath(lmm.act.c, id=0.05)

### Model reduction for c and c0
drop.scope(lmm.act.c)
drop.scope(lmm.act.c0)

lmm.act.c.2 <- update(lmm.act.c, .~. - Logstump:Moon2) # dropping the non-significant interaction term
lmm.act.c0.2 <- update(lmm.act.c0, .~. - Logstump:Moon2) # dropping the non-significant interaction term

AIC(lmm.act.c, lmm.act.c.2) #c.2 better
AIC(lmm.act.c0, lmm.act.c0.2) #c.2 better

# df          AIC                  df           AIC
# lmm.act.c   10 8640.154      lmm.act.c0   10 124568.3
# lmm.act.c.2  9 8638.156      lmm.act.c0.2  9 124567.2


# R2
performance::r2(lmm.act.c.2) # Conditional: 0.1, Marginal: 0.001
performance::r2(lmm.act.c0.2) # Conditional: 0.086, Marginal: 0.004

performance::icc(lmm.act.c.2) # Adjusted ICC: 0.099, Conditional ICC: 0.099
performance::icc(lmm.act.c0.2) # Adjusted ICC: 0.082, Conditional ICC: 0.082



### GLMM for zero-inflated continuous, hurdle models and tweedie distribution attempted
# Tweedie does not fit as well as the data is predominantly zero

try(glmm.act.a <- glmmTMB(Act_time ~ 1 + datalog + trap + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                          data = df, ziformula = ~., family=truncated_nbinom1(link = "log")))

try(glmm.act.b <- glmmTMB(Act_time ~ 1 + datalog + trap + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                          data = df, ziformula = ~., family=truncated_nbinom2(link = "log")))

try(glmm.act.c <- glmmTMB(Act_time ~ 1 + datalog + trap + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                          data = df, ziformula = ~., family=truncated_poisson(link = "log")))

# try(glmm.act.d <- glmmTMB(Act_time ~ 1 + datalog + trap + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n),
#                           data = df, ziformula = ~., family=ziGamma(link = "inverse")))

try(glmm.act.d <- glmmTMB(Act_time ~ 1 + datalog + trap + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n),
                          data = df, ziformula = ~., family=ziGamma(link = "log")))

# try(glmm.act.d <- glmmTMB(Act_time ~ 1 + datalog + trap + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n),
#                           data = df, ziformula = ~., family=tweedie(link = "log")))


AICc(glmm.act.a, glmm.act.b, glmm.act.c, glmm.act.d) # b and d have the best AIC
# Decided to use model d since it is meant for continuous data

# Test for dispersion on the best model based on AIC values
testDispersion(glmm.act.d) 
# truncated-nbinom2: no under/over-dispersion!!

# Check collinearity between variables
check_collinearity(glmm.act.d)

# QQ plot residuals and residual vs predicted
simulationOutput.act <- simulateResiduals(fittedModel = glmm.act.d)
plot(simulationOutput.act)

### Model reduction for c and c0
drop.scope(glmm.act.d)

glmm.act.d.2 <- update(glmm.act.d, .~. - Logstump:Moon2) # dropping the non-significant interaction term
glmm.act.d.3 <- update(glmm.act.d.2, .~. - trap - datalog)
glmm.act.d.4 <- update(glmm.act.d.3, .~. - Logstump)
glmm.act.d.5 <- update(glmm.act.d.4, .~. - Moon2)

#              df      AIC
# glmm.act.d   23 26649.57
# glmm.act.d.2 21 26647.28
# glmm.act.d.3 17 26645.80

AIC(glmm.act.d, glmm.act.d.2, glmm.act.d.3, glmm.act.d.4, glmm.act.d.5) # d4 has the best AIC

#              df      AIC
# glmm.act.d   23 26649.57
# glmm.act.d.2 21 26647.28
# glmm.act.d.3 17 26645.80
# glmm.act.d.4 15 26643.47
# glmm.act.d.5 13 26643.29

# Conditional model:
#   Groups   Name        Variance Std.Dev.
# ID       (Intercept) 0.06322  0.2514  
# season.n (Intercept) 0.03212  0.1792  
# Number of obs: 3604, groups:  ID, 166; season.n, 9
# 
# Zero-inflation model:
#   Groups   Name        Variance Std.Dev.
# ID       (Intercept) 0.4866   0.6976  
# season.n (Intercept) 0.1254   0.3542  
# Number of obs: 3604, groups:  ID, 166; season.n, 9
# 
# Dispersion estimate for Gamma family (sigma^2): 0.705 
# 
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     5.42445    0.08620   62.93   <2e-16 ***
# Rhodo           0.05509    0.09820    0.56    0.575    
# Moon2          -0.12072    0.08153   -1.48    0.139    
# Rhodo:Logstump  0.04950    0.05831    0.85    0.396    
# Rhodo:Moon2     0.02677    0.14555    0.18    0.854    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Zero-inflation model:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)      0.2928     0.1678   1.745 0.080939 .  
# Rhodo            0.5354     0.1497   3.576 0.000349 ***
# Moon2            0.1699     0.1332   1.276 0.202125    
# Rhodo:Logstump  -0.4513     0.1008  -4.479  7.5e-06 ***
# Rhodo:Moon2     -0.4562     0.2198  -2.075 0.037954 * 


# R2
performance::r2(glmm.act.d.4) # Conditional: 0.123, Marginal: 0.004
performance::icc(glmm.act.d.4) # Adjusted ICC: 0.119, Conditional ICC: 0.119

################################################################################################

# # LMM for distance/activity time
# lmm.act20.a <- lmer(Activity20 ~ 1 + Rhodo * Moon * Logstump + (1|ID) + (1|season.n), 
#                     REML = F,
#                     data = df1, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.act20.b <- lmer(Activity20 ~ 1 + Rhodo * Moon * Logstump + (1|ID), 
#                     REML = F,
#                     data = df1, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.act20.c <- lmer(Activity20 ~ 1 + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
#                     REML = F,
#                     data = df1, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.act20.a0 <- lmer(Activity20 ~ 1 + Rhodo * Moon * Logstump + (1|ID) + (1|season.n), 
#                      REML = F,
#                      data = df, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.act20.b0 <- lmer(Activity20 ~ 1 + Rhodo * Moon * Logstump + (1|ID), 
#                      REML = F,
#                      data = df, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.act20.c0 <- lmer(Activity20 ~ 1 + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
#                      REML = F,
#                      data = df, control=lmerControl(optimizer="bobyqa"))
# 
# 
# summary(lmm.act20.c0)
# summary(lmm.act20.c)
# anova(lmm.act20.c,lmm.act20.a)
# 
# ## Check lmm assumptions
# # Linearity
# plot(resid(lmm.act20.c),df1$Activity20)
# 
# # We need to revisit statistical significance 
# # since the assumption is that the variance is not going to differ, 
# # we would hope to see NO STATISTICAL DIFFERENCES in the following procedure 
# # Extract residual -> make them positive -> square -> anova squared residuals
# df$Model.F.Res<- residuals(lmm.act20.c0) 
# df$Model.F.Res <-abs(df$Model.F.Res) 
# df$Model.F.Res <- df$Model.F.Res^2 
# Levene.Model.F <- lm(Model.F.Res ~ ID, data=df) 
# anova(Levene.Model.F) # Non-significant for df, p: 0.67
# 
