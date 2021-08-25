################ Linear mixed effects models ################

# Load libraries
library(ggplot2)
library(ggpubr)


## Load data
# Per night per individual
df <- read.csv("../Data/pernightperID7update.csv", 
               colClasses = c(Sex = "factor", season = "factor", season.n = "factor"))

# Per individual
indi <- read.csv("../Data/capturehistoryCOMB2.csv", 
                 colClasses = c(ID = "character", ch = "character", sex = "factor", moonbin = "factor"))

# Subset out individuals that only visited one data logger
df1 <- subset(df, Diff_logger != 1)
df1.act <- subset(df1, Activity <= 20)

# Visual inspection
# str(df)
# plot(df)



################ Activity rate X Rhodo correlation test and plots ################

# Check for normal distribution, whether using Pearson is correct -> NO!
ggpubr::ggqqplot(indi$rhodo, ylab = "Rhododendron cover (%)")
ggpubr::ggqqplot(indi$spaceuse, ylab = "Rate of activity (meters/mins)")
ggpubr::ggqqplot(indi$logstump, ylab = "Quadrant log volume")

# Bit of data wrangling and subsets
indi1 <- indi[-380,]
#indi1 <- subset(indi, spaceuse != 0)
indi$spaceuse2 <- indi$spaceuse/60
inditry <- subset(indi, totalhits >1)
indihigh <- subset(indi, cover >= 0.9)
indilow <- subset(indi1, cover <= 0.1)

# Correlation tests
cor.test(indi1$rhodo, indi1$spaceuse, method = "spearman", exact = F) # R = 0.27, p = 1.278e-07
cor.test(indi1$rhodo, indi1$logstump, method = "spearman", exact = F) # R = -0.21, p = 3.603e-05

dfs1 <- subset(df, Activity20 != 0)
cor.test(dfs1$Rhodo, dfs1$Activity20, method = "spearman")
cor.test(dfs$Rhodo, dfs$Logstump, method = "spearman")

# T.test for fun
df1 <- subset(df, Activity20 != 0)
dfhigh <- subset(df1, cover > 0.9)
dflow <- subset(df1, cover < 0.1)
t.test(dfhigh$Activity20, dflow$Activity20)

t.test(indihigh$spaceuse, indilow$spaceuse)
boxplot(indihigh$spaceuse, indilow$spaceuse, ylim = c(0,4))

## Playing with non-linear
# qua_fit <- lm(spaceuse ~ poly(rhodo, 2), data = indi1)
# cub_fit <- lm(spaceuse ~ poly(rhodo, 3), data = indi1)

## First trial on cor test plots
# ggscatter(indi1, x = "rhodo", y = "spaceuse", 
#           add = "reg.line", conf.int = TRUE, 
#           cor.coef = TRUE, cor.method = "spearman",
#           xlab = "Mean rhododendron cover (%)", ylab = "Rate of activity (metres/minutes)"
#           )


## Actual cor.test plot!
# Save in the Results directory
pdf("Fig3b.pdf")

sp <- ggscatter(indi1, x = "rhodo", y = "spaceuse", 
          add = "reg.line", conf.int = TRUE, 
          ylab = "Rate of activity (metres/minutes)"
)

sp + stat_cor(aes(color="#3399FF"), show.legend = FALSE, label.y = 1,
              method = "spearman", label.x = c(0.15,0.05)) + 
  ylim(0,1) +
  xlab(expression(bolditalic("Rhododendron")~bold("cover (%)"))) +
  theme(axis.title.y=element_text(face = "bold"))

dev.off()

############################ Spatiotemporal activity GLMM plots ############################

glmmres <- read.csv("../Results/GLMMana.csv")

# Bit of data wrangling
glmmres <- glmmres[c(2:8),]
colnames(glmmres)[1] <- "Effects"
glmmres[is.na(glmmres)] <- 0

# Make duplicate
glmmres1 <- glmmres

glmmres1 <- glmmres1[,c(1,3,5,7,9,11,13,15,17)]
glmmres <- glmmres[,c(1,2,4,6,8,10,12,14,16)]

# Make data into long format
df <- data.frame(glmmres[1], stack(glmmres[2:ncol(glmmres)]))
df1 <- data.frame(glmmres1[1], stack(glmmres1[2:ncol(glmmres)]))
df2 <- cbind(df[,1:3], df1[,2:3])

# Change column names
colnames(df2)[3] <- "ID"
colnames(df2)[4] <- "sd"
colnames(df2)[5] <- "sd.ID"

# Remove rows with 0
df3 <- df2[df2$values != 0,]

# Make a duplicate to separate female and male columns for plotting
df4 <- df3


## Remove .M & .F for the ID column
# ^ - Start of the string
# (.*) - All characters but a newline up to
# .$ - The last character (captured with .) before the end of string ($).
# ..$ - The two last characters before the end of string

df4$ID <- gsub('^(.*)..$', '\\1', df4$ID)

# Create a gender column
df4$ID2 <- c(rep("M",5), rep("F", 7),
             rep("M",5), rep("F", 7),
             rep("M",4), rep("F", 4),
             rep("M",4), rep("F", 4))

# Change column names
colnames(df4)[3] <- "Indicators"
colnames(df4)[6] <- "Sex"

# Change variable name
df4$Indicators <- gsub("Total.hits", "Total.encounters", df4$Indicators)

# Bar chart plots without sex differentiation in bar colours
ggplot(df3, aes(x = values, y = Effects, fill = ID)) +
  geom_bar(stat = 'identity', position=position_dodge()) +
  geom_errorbar(aes(xmin = values - sd, xmax = values + sd),
               size = 0.5,
                position=position_dodge()) +
  theme_bw() +
  xlab("Spatiotemporal activity change (per night)") +
  ylab("") +
  scale_fill_manual("Legend", 
                    values = c("Total.hits.M" = "#999999", "Total.hits.F" = "#E69F00", 
                               "New.locations.M" = "#56B4E9", "New.locations.F" = "#009E73",
                               "Distance.M" = "#F0E442", "Distance.F" = "#0072B2",
                               "Active.time.M" = "#D55E00", "Active.time.F" = "#CC79A7")) +
  theme(legend.position = c(0.8, 0.2)) +
  
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))


## Bar chart plots with M/F and save in the Results directory
pdf("Fig4.pdf")

ggplot(df4, aes(x = values, y = Effects, color = Sex, fill = Indicators)) +
  geom_bar(stat = 'identity', position=position_dodge()) +
  geom_errorbar(aes(xmin = values - sd, xmax = values + sd),
                size = 0.5,
                position=position_dodge()) +
  theme_bw() +
  xlab("Spatiotemporal activity change (per day)") +
  ylab("") +
  # scale_fill_manual("Legend", 
  #                   values = c("Total.hits.M" = "#999999", "Total.hits.F" = "#E69F00", 
  #                              "New.locations.M" = "#56B4E9", "New.locations.F" = "#009E73",
  #                              "Distance.M" = "#F0E442", "Distance.F" = "#0072B2",
  #                              "Active.time.M" = "#D55E00", "Active.time.F" = "#CC79A7")) +
  theme(legend.position = c(0.8, 0.2)) +
  scale_fill_grey() +
  
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'),
        axis.title = element_text(face="bold"))

dev.off()

############################ Activity rate X cover GLMM plots ############################

dfm <- subset(df, Sex == "M")
dff <- subset(df, Sex == "F")

lmm.act <- lmer(Activity20 ~ 1 + Rhodo * Bamboo + (1|ID) + (1|season.n), 
                REML = F,
                data = df, control=lmerControl(optimizer="bobyqa"))


summary(lmm.act)

lmm.act.2 <- update(lmm.act, .~. - Rhodo)
lmm.act.3 <- update(lmm.act.2, .~. - Bamboo)

AIC(lmm.act.2, lmm.act, lmm.act.3)

lmm.act.4 <- lmer(Activity20 ~ 1 + Rhodo:Bamboo + (1|ID) + (1|season.n), 
                  REML = T,
                  data = df, control=lmerControl(optimizer="bobyqa"))

summary(lmm.act.4)

# try(glmm.act20 <- glmmTMB(Activity20 ~ 1 + Rhodo * Bamboo + (1|ID) + (1|season.n),
#                           data = df, ziformula = ~., family=ziGamma(link = "log")))
# 
# summary(glmm.act20)


simulationOutput.act <- simulateResiduals(fittedModel = glmm.act20)
plot(simulationOutput.act)

# library(coefplot2)
# coefplot2(lmm.act)
# 
# library(sjPlot)
# plot_model(lmm.act, type = "pred", terms = c("Rhodo", "Bamboo"))
# get_model_data(lmm.act, type = "pred", terms = c("Rhodo", "Bamboo"))

rhodbam <- data.frame(Rhodo = c(0,0.2,0.3,0.5,0.7,1),
                      Predicted = c(0.28,0.39,0.45,0.55,0.66,0.83),
                      Lower = c(0.19,0.25,0.26,0.26,0.26,0.25),
                      Upper = c(0.38,0.53,0.64,0.85,1.07,1.4))

ggplot() +
  geom_point(data = rhodbam, aes(x = Rhodo, y = Predicted)) +
  geom_line(data = rhodbam, aes(x = Rhodo, y = Predicted), size = 1) +
  geom_ribbon(data = rhodbam, aes(x = Rhodo, ymin = Lower, ymax = Upper), alpha= 0.3, fill="grey") +
  ylab("Daily activity rate (metres per minute)") +
  xlab("Rhododendron cover (%)") +
  theme_bw() +
  ggtitle("Daily activity rate under covered microhabitat structures") +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'))

# library(effects)
# eff.rhodbam <- effects::effect(term= "Rhodo:Bamboo", mod= lmm.act)
# summary(eff.rhodbam)
# 
# eff.rhodbam1 <- data.frame(eff.rhodbam)

ggplot() +
  #geom_point(data = df, aes(x = Rhodo, y = Activity20)) +
  
  geom_point(data = eff.rhodbam1, aes(x = Rhodo:Bamboo, y = fit), color="blue") +
  #4
  geom_line(data = eff.rhodbam1, aes(x = Rhodo:Bamboo, y = fit), color="blue") +
  #5
  geom_ribbon(data = eff.rhodbam1, aes(x = Rhodo:Bamboo, ymin=lower, ymax=upper), alpha= 0.3, fill="blue")

hi <- lme(spaceuse ~ rhodo*bamboo, data = indi1, random = ~1|ID)
summary(hi)

plot.lme(hi)

hi2 <- lme(Diff_logger ~ Rhodo * Logs * Logstump, data = df, random = ~1|ID)
summary(hi2)


##################################################################################################################
################################################### NOT NEEDED ###################################################
##################################################################################################################

# # Earlier versions of GLMMs analysis and reduction, they can be abandoned!

# library(nlme)
# library(lme4)
# library(lmerTest) 
# 
# # model1 <- lm(Activity20 ~ Rhodo * Moon, data = df1.act)
# # summary(model1)
# 
# # df - Logs 0.06, df1 - 0.075
# weird <- lm(Activity20 ~ Logs, data = df1)
# summary(weird)
# 
# ############# Maximal model for activity20 #############
# 
# # Maximal model
# activity20.max <- lme(Activity20 ~ Rhodo*Sex*Moon*Logs*Logstump*season, data = df1, 
#                       random = ~1|ID, method = "ML")
# summary(activity20.max)
# 
# # Simplification process - remove non-significant interactions, 
# # compare with anova, if not signficant, repeat
# 
# # First simplification
# # Remove all 6/5 way interactions as all non-significant
# activity20.2 <- lme(Activity20 ~ (Rhodo+Sex+Moon+Logs+Logstump+season)^4, data = df1,
#                     random = ~1|ID, method = "ML")
# summary(activity20.2)
# 
# anova(activity20.2, activity20.max) # p-value; 0.98, lower AIC by 21
# 
# # Second
# drop.scope(activity20.2)
# 
# activity20.3 <- update(activity20.2, .~. - Rhodo:Sex:Moon:Logs - Rhodo:Sex:Moon:Logstump 
#                        - Rhodo:Sex:Logs:Logstump - Rhodo:Sex:Logs:season - Rhodo:Sex:Logstump:season 
#                        - Rhodo:Moon:Logs:Logstump - Rhodo:Moon:Logs:season - Rhodo:Moon:Logstump:season 
#                        - Rhodo:Logs:Logstump:season - Sex:Moon:Logs:Logstump 
#                        - Sex:Moon:Logstump:season - Moon:Logs:Logstump:season)
# summary(activity20.3)
# 
# anova(activity20.2, activity20.3) # p-value: 0.98, lower AIC by 30
# 
# # Third
# drop.scope(activity20.3)
# 
# activity20.4 <- update(activity20.3, .~. - Rhodo:Sex:Logs - Rhodo:Sex:Logstump - Rhodo:Moon:Logs 
#                        - Rhodo:Moon:Logstump - Rhodo:Logs:Logstump - Rhodo:Logs:season - Rhodo:Logstump:season 
#                        - Sex:Moon:Logstump - Moon:Logs:Logstump - Moon:Logstump:season)
# summary(activity20.4)
# 
# # - Rhodo:Sex:Logs - Rhodo:Sex:Logstump - Rhodo:Moon:Logs 
# # - Rhodo:Moon:Logstump - Rhodo:Logs:Logstump - Rhodo:Logs:season - Rhodo:Logstump:season 
# # - Sex:Moon:Logstump - Moon:Logs:Logstump - Moon:Logstump:season - Rhodo:Sex:Moon:season 
# # - Sex:Moon:Logs:season - Sex:Logs:Logstump:season
# 
# anova(activity20.3, activity20.4) # p-value: 0.89, lower AIC by 19
# 
# # Fourth
# drop.scope(activity20.4)
# 
# activity20.5 <- update(activity20.4, .~. - Rhodo:Logs - Rhodo:Logstump - Moon:Logstump)
# summary(activity20.5)
# 
# anova(activity20.4, activity20.5) # p-value: 0.63, lower AIC by 4
# 
# # Fifth?
# drop.scope(activity20.5)
# 
# activity20.6 <- update(activity20.5, .~. - Rhodo:Sex:Moon:season - Sex:Moon:Logs:season - Sex:Logs:Logstump:season)
# summary(activity20.6)
# 
# anova(activity20.5, activity20.6) # p-value: 0.15, lower AIC by 3
# 
# # Sixth?
# drop.scope(activity20.6)
# 
# activity20.7 <- update(activity20.6, .~. - Rhodo:Sex:Moon - Rhodo:Moon:season 
#                        - Sex:Moon:Logs - Sex:Moon:season - Sex:Logs:Logstump - Sex:Logs:season 
#                        - Sex:Logstump:season - Moon:Logs:season - Logs:Logstump:season)
# summary(activity20.7)
# 
# anova(activity20.6, activity20.7) # p-value: 0.97, lower AIC by 23
# 
# # Seventh
# drop.scope(activity20.7)
# 
# activity20.8 <- update(activity20.7, .~. - Rhodo:Moon - Sex:Moon - Sex:Logs - Sex:Logstump - Moon:Logs 
#                        - Moon:season - Logs:Logstump - Logs:season - Logstump:season )
# summary(activity20.8)
# 
# anova(activity20.8, activity20.7) # p-value: 0.54, lower AIC by 13
# 
# # Eighth
# drop.scope(activity20.8)
# 
# activity20.9 <- update(activity20.8, .~. - Moon - Logstump)
# summary(activity20.9)
# 
# anova(activity20.8, activity20.9) # p-value: 0.25, lower AIC by 1.24
# 
# ## Explained by Rhodo, Moon and Sex!!!
# # Trial 2 is the best model out of trial1,2,3
# trial <- lme(Activity20 ~ Rhodo*Sex*Moon*Logs*Logstump, data = df1, random = ~1|ID/season)
# summary(trial)
# 
# 
# trial2 <- lme(Activity20 ~ Rhodo*season, data = dff, random = ~1|ID)
# summary(trial2)
# 
# # Assumption - linearity
# plot(resid(trial2), log(df1$Activity20))
# # Assumption - Homogeneity of Variance
# plot(trial2)
# # Residuals of model normally distributed
# qqnorm(trial2, ~ resid(.)|ID)
# 
# trial3 <- lme(Activity20 ~ Rhodo*Sex*season, data = df1.act, random = ~1|ID)
# summary(trial3)
# 
# # Bamboo
# trialbamb <- lme(Activity20 ~ Rhodo*Moon*Bamboo, data = df1, random = ~1|ID)
# summary(trialbamb)
# 
# # Microhabitat
# # Non-significant
# trialmicro <- lme(Activity20 ~ Moon * Logstump * Logs, data = df1s, random = ~1|ID/season)
# summary(trialmicro)
# 
# # # Comparing NBS to EBS and LBS
# # hi <- df1
# # hi$season <- gsub("NBS", "ABS", hi$season)
# # 
# # hitrial <- lme(Activity20 ~ Rhodo*Moon*Sex*season, data = hi, random = ~1|ID)
# # summary(hitrial)
# # 
# # hitrial2 <- lme(Activity20 ~ Rhodo*Sex*season, data = hi, random = ~1|ID)
# # summary(hitrial2)
# 
# # NBS
# # Predation risk should explain more during nbs
# # Not significant...
# # here <- lme(Activity20 ~ Rhodo*Moon, data = nbs, random = ~1|ID)
# # summary(here)
# 
# # Best so far, but none significant, 0.23 0.39 0.16
# # model2 <- lme(Activity20 ~ Rhodo * Moon, data = df1.act, random = ~1|ID)
# # summary(model2)
# 
# ###
# model3 <- lme(Total_hits ~ Rhodo * Moon*Logstump, data = dfm, random = ~1|ID/season.n)
# summary(model3)
# 
# model3micro <- lme(Total_hits ~ Logstump * Moon * Logs, data = df, random = ~1|ID/season)
# summary(model3micro)
# 
# ## Rhodo:moon 0.06
# model4 <- lme(Diff_logger ~ Rhodo * Moon*Logstump, data = dff, random = ~1|ID/season.n)
# summary(model4)
# 
# model4micro <- lme(Diff_logger ~ Logstump * Moon * Logs, data = df, random = ~1|ID/season)
# summary(model4micro)
# 
# ################ Distance ################
# 
# ### Can use if not remove single hits!!!
# model5 <- lme(Distance20 ~ Rhodo * Moon*Logstump, data = dfm1, random = ~1|ID/season.n) 
# summary(model5)
# 
# # df or df1, not sure yet
# model6 <- lme(Distance20 ~ Moon * Logs * Logstump, data = df1, random = ~1|ID/season)
# summary(model6)
# 
# ################ Activity time ################
# 
# ### Significant Moon and Rhodo*Moon
# model7 <- lme(Act_time ~ Rhodo * Moon*Logstump, data = dff, random = ~1|ID/season.n) 
# summary(model7)
# 
# ### Significant for Moon * Logs * Logstump only!! 0.03
# model8 <- lme(Act_time ~ Moon * Logs * Logstump, data = df, random = ~1|ID/season) 
# summary(model8)
# 
# 
# model9 <- lme(Act_time ~ Moon * Rhodo * Logstump, data = df1, random = ~1|ID/season) 
# summary(model9)
# 
# 
# ## Using indi from another script!
# model6 <- lme(totalhits ~ rhodo*moon, data = indi, random = ~1|ID)
# summary(model6)