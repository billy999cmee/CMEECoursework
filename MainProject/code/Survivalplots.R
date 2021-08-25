################ Survival prediction plots ################

# Load libraries
library(ggplot2)
library(grid)


################ Moon X Activity rate ################

moonspace <- read.csv("../Results/moonspaceSUR.csv")

# par(mfrow=c(2,2))
# par(mfg = c(1,1))


plot1 <- ggplot(moonspace, aes(x = rhodweight, y = logit.values)) +
  geom_line(col = 'red') +
  geom_ribbon(aes(x = rhodweight, ymin = low, ymax = high), alpha = 0.1) +
  theme_bw() +
  xlab("Activity rate under an average moonlight intensity \n(metres/minutes X 48.6%)") +
  ylab("Daily survival estimate (%)") +
  coord_cartesian(xlim = c(0,150), ylim = c(0,1)) +
  #ggtitle("Effects of activity rate under lunar illumination on wood mouse survival") +
  
  # eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank()
  ) +
  
  # draws x and y axis line
  # enlarge axis label
  theme(axis.line = element_line(color = 'black'),
        axis.title.x=element_text(size=14,face="bold"),
        axis.text=element_text(size=12))

# # remove x axis numbers
# theme(#axis.title.x=element_blank(),
#       axis.text.x=element_blank(),)
#       #axis.ticks.x=element_blank())


################ Activity rate only ################

hi2 <- read.csv("../Results/spaceuseSUR.csv")

plot2 <- ggplot(hi2, aes(x = spaceval, y = logit.values)) +
  geom_line(col = 'red', size = 0.8) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1) +
  xlab("Activity rate (metres/minutes)") +
  ylab("Daily survival estimate (%)") +
  coord_cartesian(xlim = c(0,2), ylim = c(0.92,1)) +
  #ggtitle("Effects of activity rate on wood mouse survival") +
  theme_bw() +
  
  # eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  
  # draws x and y axis line
  theme(axis.line = element_line(color = 'black'),
        axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12))

################ Activity rate X Rhododendron ################

hi3 <- read.csv("../Results/spaceuseRhodSUR.csv")

# my_title <- expression(paste("Activity rate and ", italic("Rhododendron"), " effects on woodmouse survival"))
# my_y_title <- expression(paste(italic("Rhododendron"), " cover under an average activity rate (% X 0.28 metres/mins)"))

plot3 <- ggplot(hi3, aes(x = spaceval, y = logit.values)) +
  geom_line(col = 'red', size = 0.8) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1) +
  #xlab("Rhododendron cover under average activity rate (% X 0.41 metres/mins)") +
  ylab("Daily survival estimate (%)") +
  xlab(bquote(atop(bold("An average activity rate under")~bolditalic(Rhododendron)~bold("cover"), bold("(0.28 metres/mins X %)")))) +
  coord_cartesian(ylim = c(0.97,0.99)) +
  #labs(title = my_title) +
  #ggtitle("Effects of Rhododendron with activity rate on rodent's survival") +
  #theme_bw() +
  
  # eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    # axis.text.y = element_blank(),
    # axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank()
  ) +
  
  # draws x and y axis line
  # enlarge xis title
  theme(axis.line = element_line(color = 'black'),
        axis.title.x=element_text(size=14),
        axis.text=element_text(size=12))

################ Save Fig 1a,1b,1c in the results directory ################

grid.newpage()

pdf("../Results/Fig1.pdf")
grid.draw(cbind(ggplotGrob(plot2), ggplotGrob(plot1), ggplotGrob(plot3), size = "last"))
dev.off()


################ Rhododendron X Weight ################

rhodwt <- read.csv("../Results/rhodwtSUR.csv")

# Italics
my_title <- expression(paste("Effects of ", italic("Rhododendron"), " and weight on wood mouse survival"))
my_x_title <- expression(paste("Mouse weight (grams) X ", italic("Rhododendron"), " cover (%)"))

plot4 <- ggplot(rhodwt, aes(x = rhodweight, y = logit.values)) +
  geom_line(col = 'red', size = 1) +
  geom_ribbon(aes(x = rhodweight, ymin = low, ymax = high), alpha = 0.1) +
  theme_bw() +
  xlab(bquote(atop(bold("Mouse weight (grams) X "), bolditalic("Rhododendron"~bold("cover (%)"))))) +
  ylab("Daily survival estimate (%)") +
  coord_cartesian(ylim = c(0.87,1)) +
  #ggtitle(my_title) +
  
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank(), 
    axis.ticks.y = element_blank(), 
    axis.title.y = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank()
  ) +
  
  #draws x and y axis line
  theme(axis.line.x = element_line(color = 'black'),
        axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12))

################ Rhododendron from the Rhododendron X Weight model ################

hi4 <- read.csv("../Results/rhodrhodwtSUR.csv")

# Italics
my_title <- expression(paste("Effects of ", italic("Rhododendron"), " on wood mouse survival"))
my_x_title <- expression(paste(italic("Rhododendron"), " cover (%)"))

plot5 <- ggplot(hi4, aes(x = spaceval, y = logit.values)) +
  geom_line(col = 'red', size = 0.8) +
  geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1) +
  xlab(expression(bolditalic("Rhododendron")~bold("cover (%)"))) +
  ylab("Daily survival estimate (%)") +
  coord_cartesian(ylim = c(0.87,1)) +
  #ggtitle(my_title) +
  theme_bw() +
  #eliminates background, gridlines, and chart border
  theme(
    plot.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank()
  ) +
  
  #draws x and y axis line
  theme(axis.line = element_line(color = 'black'),
        axis.title=element_text(size=14,face="bold"),
        axis.text=element_text(size=12))

########## Save Fig2a,2b in the Results directory as pdf ##########

grid.newpage()

pdf("../Results/Fig2.pdf")
grid.draw(cbind(ggplotGrob(plot5), ggplotGrob(plot4), size = "last"))
dev.off()







# ####### Rhodo only
# rhodonly <- read.csv("rhodSUR.csv")
# 
# plot4 <- ggplot(rhodonly, aes(x = rhodval, y = logit.values)) +
#   geom_line(col = 'red', size = 1) +
#   geom_ribbon(aes(ymin = low, ymax = high), alpha = 0.1) +
#   ylim(0.97,0.99) +
#   theme_bw() +
#   xlab("Rhododendron cover (%)") +
#   ylab("Daily survival estimate") +
#   ggtitle("Effects of Rhododendron on \nrodent's survival") +
#   #coord_cartesian(ylim = c(0.85,1)) +
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

