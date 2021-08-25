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

## Read data
# Per night per individual
df <- read.csv("../Data/pernightperID7update.csv", 
               colClasses = c(Sex = "factor", season = "factor", ID = "factor", season.n = "factor"))

# Remove females
df <- subset(df, Sex == "M")

# Subset out individuals that only visited one data logger
df1 <- subset(df, Diff_logger != 1)

# Convert moon illumination (%) to 0-1
df$Moon2 <- df$Moon/100
df1$Moon2 <- df1$Moon/100

# Show that data is crossed, multiple IDs in several season.n
# with(df, table(ID, season.n)) %>%
#      image(
#          col = grey.colors(10, start = 1, end = 0), 
#          axes = FALSE, 
#          xlab = "ID", 
#          ylab = "Season.n"
#      )

cat("Running all models will take hours, sit back and take a break!!!!!!!!!")
cat("Or don't run the models that take hours, they might not even be the best model!")

################################################################################################

# 5 response variables: total hits, diff-loggers, distance20, act_time, activity20
# logged - df1$act-time(right-skewed), df1$distance20(normal), df1$activity20(normal)
# Count data - total hits, diff_loggers (Poisson or quasipoisson)
# 

# Predictors - rhododendron, logs, season, sex, moon

# Problems : 2 errors in glmer, one potentially links to collnearity in fixed effects, how to fix?

# Check which model distribution with the appropirate link function fits the best 
# Determine by AIC values

################################################################################################

## Response variable: total number of hits

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

glmm.b.3 <- update(glmm.b.2, .~. - datalog - trap)

AICc(glmm.b.3, glmm.b.2, glmm.b) # b.3 best AICc

# Conditional model:
#   Groups   Name        Variance Std.Dev.
# ID       (Intercept) 0.07356  0.2712  
# season.n (Intercept) 0.02481  0.1575  
# Number of obs: 5724, groups:  ID, 234; season.n, 9
# 
# Dispersion parameter for nbinom2 family ():  104 
# 
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)     0.697137   0.063393  10.997  < 2e-16 ***
#   Rhodo          -0.126260   0.039749  -3.176  0.00149 ** 
#   Logstump       -0.016513   0.006617  -2.496  0.01257 *  
#   Moon2          -0.098598   0.036073  -2.733  0.00627 ** 
#   Rhodo:Logstump  0.138206   0.022872   6.043 1.52e-09 ***
#   Rhodo:Moon2     0.104218   0.057933   1.799  0.07203 .  

# R2
# Marginal R2: fixed effect variance/total variance (fixed,random,residual)
# indicates how much of the model variance is explained by the fixed effects only
# Conditional R2: fixed + random effect variances/total variance
# indicates how much of the model variance is explained by the complete model
# ICC: how much variance is explained by random effects only

performance::r2(glmm.b.3) # Conditional: 0.191, Marginal: 0.007
performance::icc(glmm.b.3) # Adjusted ICC: 0.185, Conditional ICC: 0.184

################################################################################################

## Response variable: total number of unique locations

# Find the nu (dispersion) parameter for Conway-Maxwell-Poisson mixed model fitting
# system.time(
#   fitme(Diff_logger ~ 1 + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
#         data = df, family = COMPoisson())
# )


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

system.time( # Adding system.time as these two families take the longest!! (One whole night)
  try(glmm.dl.h <- glmmTMB(Diff_logger ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                        data = df, family = nbinom1))
)

system.time(
  try(glmm.dl.hh <- glmmTMB(Diff_logger ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                           data = df, family = nbinom2))
)

# AIC to compare best scores: 
AICc(glmm.dl.a, glmm.dl.c, glmm.dl.d,
    glmm.dl.e, glmm.dl.f, glmm.dl.g, glmm.dl.h, glmm.dl.hh) # g,f best AICc (gamma), followed by a (genpois)

# Test for dispersion on the best model based on AIC values
testDispersion(glmm.dl.a) 
# gamma: over dispersion, 1.5517, COMp: over dispersion, 1.38
# Gen-pois: no under/over-dispersion!!

# Check collinearity between variables
check_collinearity(glmm.dl.a) # All VIF lower than 5

# # check distribution
# poisson <- fitdistr(df$Diff_logger, "gamma")
# qqp(df$Diff_logger, "gamma", lambda=Gamma$estimate, main="Poisson model")
# 
# nbinom<-fitdistr(sqrt(df$Diff_logger), "negative binomial")
# qqp(sqrt(df$Diff_logger), "nbinom", size=nbinom$estimate[[1]], 
#     mu=nbinom$estimate[[2]], main="Negative binomial model")

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
AICc(glmm.dl.a, glmm.dl.a.2, glmm.dl.a.3) # a.3 with the best AIC


#             df      AIC
# glmm.dl.a   12 14074.91
# glmm.dl.a.2 11 14073.41
# glmm.dl.a.3  9 14072.78
  
#   Conditional model:
#   Groups   Name        Variance Std.Dev.
# ID       (Intercept) 0.1121   0.3349  
# season.n (Intercept) 0.0702   0.2650  
# Number of obs: 5724, groups:  ID, 234; season.n, 9
# 
# Dispersion parameter for genpois family (): 0.526 
# 
#   Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)     0.327799   0.095620   3.428 0.000608 ***
#   Rhodo          -0.165778   0.034175  -4.851 1.23e-06 ***
#   Logstump       -0.029685   0.005999  -4.949 7.48e-07 ***
#   Moon2          -0.118610   0.029103  -4.076 4.59e-05 ***
#   Rhodo:Logstump  0.181392   0.019570   9.269  < 2e-16 ***
#   Rhodo:Moon2     0.107143   0.049304   2.173 0.029771 *  

# R2
performance::r2(glmm.dl.a.3) # Conditional: 0.353, Marginal: 0.013
performance::icc(glmm.dl.a.2) # Adjusted ICC: 0.354, Conditional ICC: 0.346


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

# ## Response variable: toal distance travelled
# 
# # LMM for Distance
# lmm.dis.a <- lmer(log(Distance20) ~ 1 + datalog+trap+ Rhodo * Moon * Logstump + (1|ID) + (1|season.n), 
#                   REML = F,
#                   data = df1, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.dis.b <- lmer(log(Distance20) ~ 1 + datalog+trap+ Rhodo * Moon * Logstump + (1|ID), 
#                   REML = F,
#                   data = df1, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.dis.c <- lmer(log(Distance20) ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
#                   REML = F,
#                   data = df1, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.dis.a0 <- lmer(Distance20 ~ 1 + datalog+trap+ Rhodo * Moon * Logstump + (1|ID) + (1|season.n), 
#                   REML = F,
#                   data = df, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.dis.b0 <- lmer(Distance20 ~ 1 + datalog+trap+ Rhodo * Moon * Logstump + (1|ID), 
#                   REML = F,
#                   data = df, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.dis.c0 <- lmer(Distance20 ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
#                   REML = F,
#                   data = df, control=lmerControl(optimizer="bobyqa"))
# 
# AIC(lmm.dis.a, lmm.dis.b, lmm.dis.c) # c best
# AIC(lmm.dis.a0, lmm.dis.b0, lmm.dis.c0)
# 
# summary(lmm.dis.c)
# summary(lmm.dis.c0)
# # anova(lmm.dis.c,lmm.dis.a)
# 
# ## Check lmm assumptions
# # Linearity
# plot(resid(lmm.dis.c),log(df1$Distance20))
# plot(resid(lmm.dis.c0), df$Distance20)
# 
# # For this portion of the analysis, we need to revisit about statistical significance 
# # - since the assumption is that the variance is not going to differ, 
# # we would hope to see NO STATISTICAL DIFFERENCES in the following procedure (i.e. p>0.05) to confirm that -
# 
# df1$Model.F.Res<- residuals(lmm.dis.c) #extracts the residuals and places them in a new column in our original data table
# df1$Model.F.Res <-abs(df1$Model.F.Res) #creates a new column with the absolute value of the residuals
# df1$Model.F.Res <- df1$Model.F.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
# Levene.Model.F <- lm(Model.F.Res ~ ID, data=df1) #ANOVA of the squared residuals
# anova(Levene.Model.F) # Non-significant!!
# 
# #
# # qqmath(lmm.dis.c, id=0.05)
# 
# ### Model reduction
# drop.scope(lmm.dis.c)
# 
# lmm.dis.c.2 <- update(lmm.dis.c, .~. - Logstump:Moon2) # dropping the non-significant interaction term
# 
# AIC(lmm.dis.c, lmm.dis.c.2) #c.2 better
# 
# # df      AIC
# # lmm.dis.c   10 7208.310
# # lmm.dis.c.2  9 7206.379
# 
# # R2
# performance::r2(lmm.dis.c.2) # Conditional: 0.249, Marginal: 0.021
# performance::icc(lmm.dis.c.2) # Adjusted ICC: 0.233, Conditional ICC: 0.228

################################################################################################

## Response variable: toal distance travelled

## GLMM for df, accounting for all zeros!
# Not using tweedie as data is predominantly zero

try(glmm.dis.a <- glmmTMB(Distance20 ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                          data = df, ziformula = ~., family=truncated_nbinom1(link = "log")))

try(glmm.dis.b <- glmmTMB(Distance20 ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                          data = df, ziformula = ~., family=truncated_nbinom2(link = "log")))

try(glmm.dis.c <- glmmTMB(Distance20 ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                          data = df, ziformula = ~., family=truncated_poisson(link = "log")))

try(glmm.dis.d <- glmmTMB(Distance20 ~ 1 + datalog + trap + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                          data = df, ziformula = ~., family=ziGamma(link = "log")))

# try(glmm.dis.dd <- glmmTMB(Distance20 ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
#                           data = df, ziformula = ~., family=tweedie(link = "log")))
# 
# try(glmm.dis.d <- bcplm(Distance20 ~ 1 + datalog+trap+ (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n),
#                     data = df, tune.iter = 2000,
#                     n.iter = 2000, n.burnin = 500, n.thin = 5))

AICc(glmm.dis.a, glmm.dis.b, glmm.dis.c, glmm.dis.d) # d best AIC, followed by b

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
glmm.dis.d.3 <- update(glmm.dis.d.2, .~. - trap - datalog)
glmm.dis.d.4 <- update(glmm.dis.d.3, .~. - Moon2)

AICc(glmm.dis.d, glmm.dis.d.2, glmm.dis.d.3, glmm.dis.d.4) # 3 best

#              df      AIC
# glmm.dis.d   23 38301.63
# glmm.dis.d.2 21 38298.10
# glmm.dis.d.3 17 38291.83



# Zero-inflation model:
#   Groups   Name        Variance Std.Dev.
# ID       (Intercept) 0.7691   0.8770  
# season.n (Intercept) 0.1983   0.4453  
# Number of obs: 5724, groups:  ID, 234; season.n, 9
# 
# Dispersion estimate for Gamma family (sigma^2): 0.502 
# 
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)     4.32247    0.12125   35.65  < 2e-16 ***
#   Rhodo          -0.30547    0.07032   -4.34 1.40e-05 ***
#   Logstump       -0.04869    0.01169   -4.16 3.12e-05 ***
#   Moon2          -0.09620    0.05653   -1.70   0.0888 .  
#   Rhodo:Logstump  0.34748    0.04676    7.43 1.08e-13 ***
#   Rhodo:Moon2     0.10900    0.09616    1.13   0.2570    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Zero-inflation model:
#                 Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     0.35470    0.18810   1.886  0.05934 .  
# Rhodo           0.37517    0.12331   3.043  0.00235 ** 
# Logstump        0.03727    0.01945   1.917  0.05529 .  
# Moon2           0.18392    0.11050   1.664  0.09603 .  
# Rhodo:Logstump -0.42307    0.08371  -5.054 4.32e-07 ***
# Rhodo:Moon2    -0.32120    0.17696  -1.815  0.06951 .  

# R2
performance::r2(glmm.dis.d.3) # Conditional: 0.347, Marginal: 0.030
performance::icc(glmm.dis.d.3) # Adjusted ICC: 0.327, Conditional ICC: 0.317


################################################################################################

# ## Response variable: toal active time
# 
# # LMM for activity time, using two data sets: df and df1 (without zeros)
# lmm.act.a <- lmer(log(Act_time) ~ 1 + Rhodo * Moon * Logstump + (1|ID) + (1|season.n), 
#                   REML = F,
#                   data = df1, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.act.b <- lmer(log(Act_time) ~ 1 + Rhodo * Moon * Logstump + (1|ID), 
#                   REML = F,
#                   data = df1, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.act.c <- lmer(log(Act_time) ~ 1 + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
#                   REML = F,
#                   data = df1, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.act.a0 <- lmer(Act_time ~ 1 + Rhodo * Moon * Logstump + (1|ID) + (1|season.n), 
#                    REML = F,
#                    data = df, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.act.b0 <- lmer(Act_time ~ 1 + Rhodo * Moon * Logstump + (1|ID), 
#                    REML = F,
#                    data = df, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.act.c0 <- lmer(Act_time ~ 1 + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
#                    REML = F,
#                    data = df, control=lmerControl(optimizer="bobyqa"))
# 
# AIC(lmm.act.a, lmm.act.b, lmm.act.c)
# AIC(lmm.act.a0, lmm.act.b0, lmm.act.c0)
# 
# summary(lmm.act.c0)
# summary(lmm.act.c)
# anova(lmm.act.c,lmm.act.a)
# 
# ## Check lmm assumptions
# # Linearity
# plot(resid(lmm.act.c0),df$Act_time)
# 
# # We need to revisit statistical significance 
# # since the assumption is that the variance is not going to differ, 
# # we would hope to see NO STATISTICAL DIFFERENCES in the following procedure 
# # Extract residual -> make them positive -> square -> anova squared residuals
# df$Model.F.Res<- residuals(lmm.act.c0) 
# df$Model.F.Res <-abs(df$Model.F.Res) 
# df$Model.F.Res <- df$Model.F.Res^2 
# Levene.Model.F <- lm(Model.F.Res ~ ID, data=df) 
# anova(Levene.Model.F) # Non-significant for df, p:0.5!
# 
# #
# # qqmath(lmm.act.c, id=0.05)
# 
# ### Model reduction for c and c0
# drop.scope(lmm.act.c)
# drop.scope(lmm.act.c0)
# 
# lmm.act.c.2 <- update(lmm.act.c, .~. - Logstump:Moon2) # dropping the non-significant interaction term
# lmm.act.c0.2 <- update(lmm.act.c0, .~. - Logstump:Moon2) # dropping the non-significant interaction term
# 
# AIC(lmm.act.c, lmm.act.c.2) #c.2 better
# AIC(lmm.act.c0, lmm.act.c0.2) #c.2 better
# 
# # df          AIC                  df           AIC
# # lmm.act.c   10 8640.154      lmm.act.c0   10 124568.3
# # lmm.act.c.2  9 8638.156      lmm.act.c0.2  9 124567.2
# 
# 
# # R2
# performance::r2(lmm.act.c.2) # Conditional: 0.1, Marginal: 0.001
# performance::r2(lmm.act.c0.2) # Conditional: 0.086, Marginal: 0.004
# 
# performance::icc(lmm.act.c.2) # Adjusted ICC: 0.099, Conditional ICC: 0.099
# performance::icc(lmm.act.c0.2) # Adjusted ICC: 0.082, Conditional ICC: 0.082

################################################################################################

## Response variable: toal active time

### GLMM for zero-inflated continuous, hurdle models and tweedie distribution attempted

try(glmm.act.a <- glmmTMB(Act_time ~ 1 + datalog + trap + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                         data = df, ziformula = ~., family=truncated_nbinom1(link = "log")))

try(glmm.act.b <- glmmTMB(Act_time ~ 1 + datalog + trap + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                          data = df, ziformula = ~., family=truncated_nbinom2(link = "log")))

try(glmm.act.c <- glmmTMB(Act_time ~ 1 + datalog + trap + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                          data = df, ziformula = ~., family=truncated_poisson(link = "log")))

try(glmm.act.d <- glmmTMB(Act_time ~ 1 + datalog + trap + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
                          data = df, ziformula = ~., family=ziGamma(link = "log")))


AICc(glmm.act.a, glmm.act.b, glmm.act.c, glmm.act.d) # b and d have the best AICs
# Using model d since its for continuous data

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
# glmm.act.d   23 46010.72
# glmm.act.d.2 21 46007.46
# glmm.act.d.3 17 46006.24

AICc(glmm.act.d, glmm.act.d.2, glmm.act.d.3, glmm.act.d.4, glmm.act.d.5) # d3/d4

# df      AIC
# glmm.act.d   23 46010.72
# glmm.act.d.2 21 46007.46
# glmm.act.d.3 17 46006.24
# glmm.act.d.4 15 46006.11
# glmm.act.d.5 13 46005.40

# Conditional model:
#   Groups   Name        Variance Std.Dev.
# ID       (Intercept) 0.0362   0.1903  
# season.n (Intercept) 0.0287   0.1694  
# Number of obs: 5724, groups:  ID, 234; season.n, 9
# 
# Zero-inflation model:
#   Groups   Name        Variance Std.Dev.
# ID       (Intercept) 0.7934   0.8907  
# season.n (Intercept) 0.1970   0.4438  
# Number of obs: 5724, groups:  ID, 234; season.n, 9
# 
# Dispersion estimate for Gamma family (sigma^2): 0.661 
# 
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)     5.42921    0.07280   74.58   <2e-16 ***
# Rhodo           0.02234    0.07026    0.32    0.750    
# Moon2          -0.03738    0.06179   -0.60    0.545    
# Rhodo:Logstump  0.06689    0.04285    1.56    0.118    
# Rhodo:Moon2     0.11109    0.10429    1.07    0.287    
# ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Zero-inflation model:
#   Estimate Std. Error z value Pr(>|z|)    
#   (Intercept)     0.37177    0.18686   1.990  0.04664 *  
#   Rhodo           0.32610    0.12043   2.708  0.00678 ** 
#   Moon2           0.18874    0.11053   1.708  0.08771 .  
#   Rhodo:Logstump -0.37118    0.08068  -4.601 4.21e-06 ***
#   Rhodo:Moon2    -0.33458    0.17709  -1.889  0.05885 .  



# R2
performance::r2(glmm.act.d.4) # Conditional: 0.093, Marginal: 0.004
performance::icc(glmm.act.d.4) # Adjusted ICC: 0.089, Conditional ICC: 0.089

################################################################################################

# # LMM for distance/activity time
# lmm.act20.a <- lmer(Activity20 ~ 1 + Rhodo * Moon * Logstump + (1|ID) + (1|season.n), 
#                   REML = F,
#                   data = df1, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.act20.b <- lmer(Activity20 ~ 1 + Rhodo * Moon * Logstump + (1|ID), 
#                   REML = F,
#                   data = df1, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.act20.c <- lmer(Activity20 ~ 1 + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
#                   REML = F,
#                   data = df1, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.act20.a0 <- lmer(Activity20 ~ 1 + Rhodo * Moon * Logstump + (1|ID) + (1|season.n), 
#                    REML = F,
#                    data = df, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.act20.b0 <- lmer(Activity20 ~ 1 + Rhodo * Moon * Logstump + (1|ID), 
#                    REML = F,
#                    data = df, control=lmerControl(optimizer="bobyqa"))
# 
# lmm.act20.c0 <- lmer(Activity20 ~ 1 + (Rhodo + Logstump + Moon2)^2 + (1|ID) + (1|season.n), 
#                    REML = F,
#                    data = df, control=lmerControl(optimizer="bobyqa"))
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
