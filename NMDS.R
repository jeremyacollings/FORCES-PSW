########## ORDINATIONS FOR FORCES PSW PROJECT ##########

# In this script, I have run all ordinations for the FORCES PSW Project. These ordinations are meant
# to visualize patterns in community composition across our nested experimental design (plots within
# areas within sites across two years). 

library(tidyverse)
library(vegan)
library(patchwork)

##### I. WHOLE PLANT COMMUNITY

### PREPPING DATA FOR THE NMDS

veg <- mydata$veg_noPSW
veg$quadrat <- c(1:324)
veg <- veg[, c(86, 1:83)]

plot_type <- as.factor(paste(mydata$identifiers$site, # creating plot level ID
                             mydata$identifiers$area, 
                             mydata$identifiers$plot, 
                             sep = ""))

year <- c(rep(1, times = 162), rep(3, times = 162))

veg_dat <- cbind.data.frame("site" = mydata$identifiers$site, "area" = mydata$identifiers$area, 
                            "plot" = mydata$identifiers$plot, "plot_type" = plot_type,
                            "year" = year, "quad" = veg$quadrat, round(veg[2:84]))

deleted_rows <- which(rowSums(veg_dat[, 7:89]) == 0) # Taking out rows with 0% total cover

veg_dat <- veg_dat[-deleted_rows,] 

### PERFORMING THE NMDS

# m1 <- metaMDS(veg_dat[, 7:89], k = 2, trymax = 100)
# m1
# plot(m1)

# quad 114 seems to be quite a multivariate outlier

#m2 <- metaMDS(veg_dat[-c(103), 7:89], k = 2, trymax = 100) #quad 114 is in the 103rd row
#m2
#plot(m2, type = "t")

# quads 22 and 84 are now sorta outliers

 m3 <- metaMDS(veg_dat[-c(19, 78, 103), 7:89], k = 2, trymax = 100)
 m3
 plot(m3, type = "t")


### PREPPING SITE-YEAR DATA

# all the necessary variables
com.scores <- as.data.frame(scores(m3))
com.scores$year <- veg_dat[-c(19, 78, 103), 5]
com.scores$site <- veg_dat[-c(19, 78, 103), 1]
com.scores$area <- veg_dat[-c(19, 78, 103), 2]
com.scores$plot <- veg_dat[-c(19, 78, 103), 3]
com.scores$site.year <- as.factor(paste(com.scores$site, com.scores$year, sep = ""))
com.scores$area.year <- as.factor(paste(com.scores$area, com.scores$year, sep = ""))
com.scores$plot.year <- as.factor(paste(com.scores$plot, com.scores$year, sep = ""))

#new datasets by site
com.scores.TG <- com.scores[which(com.scores$site == "TG"), ]
com.scores.BM <- com.scores[which(com.scores$site == "BM"), ]
com.scores.GL <- com.scores[which(com.scores$site == "GL"), ]

# The following code creates the centroids and the standard deviation for data points averaged 
# by site and year. 

NMDS1.site.year.mean <- aggregate(NMDS1 ~ site.year, com.scores, mean) # x coordinate of centroid
NMDS1.site.year.sd <- aggregate(NMDS1 ~ site.year, com.scores, sd) # x error bar
NMDS2.site.year.mean <- aggregate(NMDS2 ~ site.year, com.scores, mean) # y coordinate of centroid
NMDS2.site.year.sd <- aggregate(NMDS2 ~ site.year, com.scores, sd) # y error bar
NMDS.site.year <- cbind.data.frame(NMDS1.site.year.mean, NMDS1.site.year.sd$NMDS1,
                                   NMDS2.site.year.mean$NMDS2, NMDS2.site.year.sd$NMDS2)

NMDS.site.year <- plyr::rename(NMDS.site.year, c("site.year" = "site.year", "NMDS1" = "NMDS1.mean",
                                           "NMDS1.site.year.sd$NMDS1" = "NMDS1.sd",
                                           "NMDS2.site.year.mean$NMDS2" = "NMDS2.mean",  
                                           "NMDS2.site.year.sd$NMDS2" = "NMDS2.sd"))
NMDS.site.year$site <- c("BM", "BM", "GL", "GL", "TG", "TG")
NMDS.site.year$year <- c(1, 3, 1, 3, 1, 3)

### SITE-YEAR GRAPH

ggplot() + geom_point(data = com.scores, aes(x = NMDS1, y = NMDS2, color = site, shape = as.factor(year)), alpha = 0.5) +
  geom_point(data = NMDS.site.year, aes(x = NMDS1.mean, y = NMDS2.mean, color = site, shape = as.factor(year)), size = 4) + 
  geom_errorbar(data = NMDS.site.year, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = NMDS.site.year, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +           
  scale_color_manual(name = "Site", values = c("BM" = "royalblue4", "GL" = "goldenrod", "TG" = "green4")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17)) +
  theme_classic(base_size = 12) +
  #scale_x_continuous(limits = c(-.75, .75)) +
  #scale_y_continuous(limits = c(-.6, .6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

ggsave("NMDS by Site and Year.pdf")

### PREPPING AREA-PLOT DATA

# The following code creates the centroids and the standard deviation for data points averaged 
# by area or plot and year and site. 

# For BM:
# By Area: 

BM.NMDS1.area.mean <- aggregate(NMDS1 ~ area.year, com.scores.BM, mean)
BM.NMDS1.area.sd <- aggregate(NMDS1 ~ area.year, com.scores.BM, sd)
BM.NMDS2.area.mean <- aggregate(NMDS2 ~ area.year, com.scores.BM, mean)
BM.NMDS2.area.sd <- aggregate(NMDS2 ~ area.year, com.scores.BM, sd)
BM.NMDS.area <- cbind.data.frame(BM.NMDS1.area.mean, BM.NMDS1.area.sd$NMDS1,
                                 BM.NMDS2.area.mean$NMDS2, BM.NMDS2.area.sd$NMDS2)

BM.NMDS.area <- plyr::rename(BM.NMDS.area, c("area.year" = "area.year", 
                                       "NMDS1" = "NMDS1.mean", 
                                       "BM.NMDS1.area.sd$NMDS1" = "NMDS1.sd", 
                                       "BM.NMDS2.area.mean$NMDS2" = "NMDS2.mean", 
                                       "BM.NMDS2.area.sd$NMDS2" = "NMDS2.sd"))
BM.NMDS.area$area <- c("M", "M", "R", "R")
BM.NMDS.area$year <- c(1, 3, 1, 3)
BM.NMDS.area$level <- rep("Area", dim(BM.NMDS.area)[[1]])
BM.NMDS.area$site <- rep("BM", dim(BM.NMDS.area)[[1]])

# By plot:
BM.NMDS1.plot.mean <- aggregate(NMDS1 ~ plot.year, com.scores.BM, mean)
BM.NMDS1.plot.sd <- aggregate(NMDS1 ~ plot.year, com.scores.BM, sd)
BM.NMDS2.plot.mean <- aggregate(NMDS2 ~ plot.year, com.scores.BM, mean)
BM.NMDS2.plot.sd <- aggregate(NMDS2 ~ plot.year, com.scores.BM, sd)
BM.NMDS.plot <- cbind.data.frame(BM.NMDS1.plot.mean, BM.NMDS1.plot.sd$NMDS1,
                                 BM.NMDS2.plot.mean$NMDS2, BM.NMDS2.plot.sd$NMDS2)

BM.NMDS.plot <- plyr::rename(BM.NMDS.plot, c("plot.year" = "plot.year", 
                                       "NMDS1" = "NMDS1.mean", 
                                       "BM.NMDS1.plot.sd$NMDS1" = "NMDS1.sd", 
                                       "BM.NMDS2.plot.mean$NMDS2" = "NMDS2.mean", 
                                       "BM.NMDS2.plot.sd$NMDS2" = "NMDS2.sd"))
BM.NMDS.plot$plot <- c("F", "F", "N", "N", "O", "O")
BM.NMDS.plot$year <- c(1, 3, 1, 3, 1, 3)
BM.NMDS.plot$level<-rep("Plot", dim(BM.NMDS.plot)[[1]])
BM.NMDS.plot$site<-rep("BM", dim(BM.NMDS.plot)[[1]])

# For GL:
# By area:
GL.NMDS1.area.mean <- aggregate(NMDS1 ~ area.year, com.scores.GL, mean)
GL.NMDS1.area.sd <- aggregate(NMDS1 ~ area.year, com.scores.GL, sd)
GL.NMDS2.area.mean <- aggregate(NMDS2 ~ area.year, com.scores.GL, mean)
GL.NMDS2.area.sd <- aggregate(NMDS2 ~ area.year, com.scores.GL, sd)
GL.NMDS.area <- cbind.data.frame(GL.NMDS1.area.mean, GL.NMDS1.area.sd$NMDS1,
                                 GL.NMDS2.area.mean$NMDS2, GL.NMDS2.area.sd$NMDS2)

GL.NMDS.area <- plyr::rename(GL.NMDS.area, c("area.year" = "area.year", 
                                       "NMDS1" = "NMDS1.mean", 
                                       "GL.NMDS1.area.sd$NMDS1" = "NMDS1.sd", 
                                       "GL.NMDS2.area.mean$NMDS2" = "NMDS2.mean", 
                                       "GL.NMDS2.area.sd$NMDS2" = "NMDS2.sd"))
GL.NMDS.area$area <- c("M", "M", "R", "R")
GL.NMDS.area$year <- c(1, 3, 1, 3)
GL.NMDS.area$level<-rep("Area", dim(GL.NMDS.area)[[1]])
GL.NMDS.area$site<-rep("GL", dim(GL.NMDS.area)[[1]])

# By plot:
GL.NMDS1.plot.mean <- aggregate(NMDS1 ~ plot.year, com.scores.GL, mean)
GL.NMDS1.plot.sd <- aggregate(NMDS1 ~ plot.year, com.scores.GL, sd)
GL.NMDS2.plot.mean <- aggregate(NMDS2 ~ plot.year, com.scores.GL, mean)
GL.NMDS2.plot.sd <- aggregate(NMDS2 ~ plot.year, com.scores.GL, sd)
GL.NMDS.plot <- cbind.data.frame(GL.NMDS1.plot.mean, GL.NMDS1.plot.sd$NMDS1,
                                 GL.NMDS2.plot.mean$NMDS2, GL.NMDS2.plot.sd$NMDS2)

GL.NMDS.plot <- plyr::rename(GL.NMDS.plot, c("plot.year" = "plot.year", 
                                       "NMDS1" = "NMDS1.mean", 
                                       "GL.NMDS1.plot.sd$NMDS1" = "NMDS1.sd", 
                                       "GL.NMDS2.plot.mean$NMDS2" = "NMDS2.mean", 
                                       "GL.NMDS2.plot.sd$NMDS2" = "NMDS2.sd"))
GL.NMDS.plot$plot <- c("F", "F", "N", "N", "O", "O")
GL.NMDS.plot$year <- c(1, 3, 1, 3, 1, 3)
GL.NMDS.plot$level<-rep("Plot", dim(GL.NMDS.plot)[[1]])
GL.NMDS.plot$site<-rep("GL", dim(GL.NMDS.plot)[[1]])

# For TG
# By area:
TG.NMDS1.area.mean <- aggregate(NMDS1 ~ area.year, com.scores.TG, mean)
TG.NMDS1.area.sd <- aggregate(NMDS1 ~ area.year, com.scores.TG, sd)
TG.NMDS2.area.mean <- aggregate(NMDS2 ~ area.year, com.scores.TG, mean)
TG.NMDS2.area.sd <- aggregate(NMDS2 ~ area.year, com.scores.TG, sd)
TG.NMDS.area <- cbind.data.frame(TG.NMDS1.area.mean, TG.NMDS1.area.sd$NMDS1,
                                 TG.NMDS2.area.mean$NMDS2, TG.NMDS2.area.sd$NMDS2)

TG.NMDS.area <- plyr::rename(TG.NMDS.area, c("area.year" = "area.year", 
                                       "NMDS1" = "NMDS1.mean", 
                                       "TG.NMDS1.area.sd$NMDS1" = "NMDS1.sd", 
                                       "TG.NMDS2.area.mean$NMDS2" = "NMDS2.mean", 
                                       "TG.NMDS2.area.sd$NMDS2" = "NMDS2.sd"))
TG.NMDS.area$area <- c("M", "M", "R", "R")
TG.NMDS.area$year <- c(1, 3, 1, 3)
TG.NMDS.area$level<-rep("Area", dim(TG.NMDS.area)[[1]])
TG.NMDS.area$site<-rep("TG", dim(TG.NMDS.area)[[1]])

# By plot: 
TG.NMDS1.plot.mean <- aggregate(NMDS1 ~ plot.year, com.scores.TG, mean)
TG.NMDS1.plot.sd <- aggregate(NMDS1 ~ plot.year, com.scores.TG, sd)
TG.NMDS2.plot.mean <- aggregate(NMDS2 ~ plot.year, com.scores.TG, mean)
TG.NMDS2.plot.sd <- aggregate(NMDS2 ~ plot.year, com.scores.TG, sd)
TG.NMDS.plot <- cbind.data.frame(TG.NMDS1.plot.mean, TG.NMDS1.plot.sd$NMDS1,
                                 TG.NMDS2.plot.mean$NMDS2, TG.NMDS2.plot.sd$NMDS2)

TG.NMDS.plot <- plyr::rename(TG.NMDS.plot, c("plot.year" = "plot.year", 
                                       "NMDS1" = "NMDS1.mean", 
                                       "TG.NMDS1.plot.sd$NMDS1" = "NMDS1.sd", 
                                       "TG.NMDS2.plot.mean$NMDS2" = "NMDS2.mean", 
                                       "TG.NMDS2.plot.sd$NMDS2" = "NMDS2.sd"))
TG.NMDS.plot$plot <- c("F", "F", "N", "N", "O", "O")
TG.NMDS.plot$year <- c(1, 3, 1, 3, 1, 3)
TG.NMDS.plot$level<-rep("Plot", dim(TG.NMDS.plot)[[1]])
TG.NMDS.plot$site<-rep("TG", dim(TG.NMDS.plot)[[1]])

### AREA-PLOT GRAPH

BM.A <- ggplot() + geom_point(data = com.scores.BM, aes(x = NMDS1, y = NMDS2, color = area, shape = as.factor(year)), alpha = 0.4) +
  geom_point(data = BM.NMDS.area, aes(x = NMDS1.mean, y = NMDS2.mean, color = area, shape = as.factor(year)), size = 4, alpha = 1) +
  geom_errorbar(data = BM.NMDS.area, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = BM.NMDS.area, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +
  scale_color_manual(name = "Area", labels = c("M" = "Management", "R" = "Reference"), 
                     values = c("M" = "gray53", "R" = "darkorange1")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17), labels = c("1" = "2018", "3" = "2020")) +
  theme(axis.text.y   = element_text(size=12),
        axis.text.x   = element_text(size=12),
        axis.title.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(color="black", fill="white"), 
        legend.key = element_rect(colour = NA, fill = NA)) + 
  xlab(element_blank()) + 
  scale_x_continuous(limits = c(-.8, .5), breaks = c(-.8, -.475, -0.15, 0.175, .5)) +
  scale_y_continuous(limits = c(-.5, .5), breaks = c(-.5, -.25, 0, .25, .5)) +
  facet_grid(. ~ level)

BM.P <- ggplot() + geom_point(data = com.scores.BM, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.4) +
  geom_point(data = BM.NMDS.plot, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4, alpha = 1) +
  geom_errorbar(data = BM.NMDS.plot, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = BM.NMDS.plot, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +
  scale_color_manual(name = "Plot", labels = c("F" = "Fenced", "N" = "No V. rossicum", "O" = "Open"), 
                     values = c("F" = "royalblue4", "N" = "goldenrod", "O" = "green4")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17), labels = c("1" = "2018", "3" = "2020")) +
  theme(axis.text.y   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x   = element_text(size=12),
        axis.title.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(color="black", fill="white"), 
        legend.key = element_rect(colour = NA, fill = NA)) + 
  xlab(element_blank()) + ylab(element_blank()) + 
  scale_x_continuous(limits = c(-.75, .25), breaks = c(-.75, -.5, -.25, 0, .25)) +
  scale_y_continuous(limits = c(-.5, .5), breaks = c(-.5, -.25, 0, .25, .5)) +
  facet_grid(site ~ level)

GL.A <- ggplot() + geom_point(data = com.scores.GL, aes(x = NMDS1, y = NMDS2, color = area, shape = as.factor(year)), alpha = 0.4) +
  geom_point(data = GL.NMDS.area, aes(x = NMDS1.mean, y = NMDS2.mean, color = area, shape = as.factor(year)), size = 4, alpha = 1) +
  geom_errorbar(data = GL.NMDS.area, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = GL.NMDS.area, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +
  scale_color_manual(name = "Area", labels = c("M" = "Management", "R" = "Reference"), 
                     values = c("M" = "gray53", "R" = "darkorange1")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17), labels = c("1" = "2018", "3" = "2020")) +
  theme(axis.text.y   = element_text(size=12),
        axis.text.x   = element_text(size=12),
        axis.title.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(color="black", fill="white"), 
        legend.key = element_rect(colour = NA, fill = NA)) + 
  xlab(element_blank()) +
  scale_x_continuous(limits = c(-.25, .75), breaks = c(-.25, 0, .25, .5, .75)) +
  scale_y_continuous(limits = c(-.5, .5), breaks = c(-.5, -.25, 0, .25, .5))

GL.P <- ggplot() + geom_point(data = com.scores.GL, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.4) +
  geom_point(data = GL.NMDS.plot, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4, alpha = 1) +
  geom_errorbar(data = GL.NMDS.plot, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = GL.NMDS.plot, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +
  scale_color_manual(name = "Plot", labels = c("F" = "Fenced", "N" = "No V. rossicum", "O" = "Open"), 
                     values = c("F" = "royalblue4", "N" = "goldenrod", "O" = "green4")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17), labels = c("1" = "2018", "3" = "2020")) +
  theme(axis.text.y   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x   = element_text(size=12),
        axis.title.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(color="black", fill="white"), 
        legend.key = element_rect(colour = NA, fill = NA)) + 
  xlab(element_blank()) + ylab(element_blank()) + 
  #scale_x_continuous(limits = c(-1, .5), breaks = c(-.25, 0, .25, .5, .75)) +
  #scale_y_continuous(limits = c(-.5, .5), breaks = c(-.5, -.25, 0, .25, .5)) +
  facet_grid(site ~ .)

TG.A <- ggplot() + geom_point(data = com.scores.TG, aes(x = NMDS1, y = NMDS2, color = area, shape = as.factor(year)), alpha = 0.4) +
  geom_point(data = TG.NMDS.area, aes(x = NMDS1.mean, y = NMDS2.mean, color = area, shape = as.factor(year)), size = 4, alpha = 1) +
  geom_errorbar(data = TG.NMDS.area, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = TG.NMDS.area, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +
  scale_color_manual(name = "Area", labels = c("M" = "Management", "R" = "Reference"), 
                     values = c("M" = "gray53", "R" = "darkorange1")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17), labels = c("1" = "2018", "3" = "2020")) +
  theme(axis.text.y   = element_text(size=12),
        axis.text.x   = element_text(size=12),
        axis.title.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(color="black", fill="white"), 
        legend.key = element_rect(colour = NA, fill = NA)) + 
  scale_x_continuous(limits = c(-1.7, 1), breaks = c(-1.4, -.8, -.2, .4, 1)) +
  scale_y_continuous(limits = c(-.8, .6), breaks = c(-.6, -.3, 0, .3, .6))

TG.P <- ggplot() + geom_point(data = com.scores.TG, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.4) +
  geom_point(data = TG.NMDS.plot, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4, alpha = 1) +
  geom_errorbar(data = TG.NMDS.plot, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = TG.NMDS.plot, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +
  scale_color_manual(name = "Plot", labels = c("F" = "Fenced", "N" = "No V. rossicum", "O" = "Open"), 
                     values = c("F" = "royalblue4", "N" = "goldenrod", "O" = "green4")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17), labels = c("1" = "2018", "3" = "2020")) +
  theme(axis.text.y   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x   = element_text(size=12),
        axis.title.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        strip.background = element_rect(color="black", fill="white"),
        legend.key = element_rect(colour = NA, fill = NA)) + 
  ylab(element_blank()) +
  scale_x_continuous(limits = c(-1.7, 1), breaks = c(-1.4, -.8, -.2, .4, 1)) +
  scale_y_continuous(limits = c(-.8, .6), breaks = c(-.6, -.3, 0, .3, .6)) +
  facet_grid(site ~ .)


(BM.A | BM.P) / (GL.A | GL.P) / (TG.A | TG.P) + plot_layout(guides="collect")

ggsave("NMDS by Area and Plot.pdf")











##### II. ONLY NON-INVASIVE PLANTS

### PREPPING DATA FOR NMDS

# take out invasive species

veg_dat_noinv <- veg_dat[, -c(12, 19, 28, 46, 50, 69, 70)]

deleted_rows2 <- which(rowSums(veg_dat_noinv[, 7:82]) == 0) # Taking out rows with 0% total cover

veg_dat_noinv <- veg_dat_noinv[-deleted_rows2,] 

### PERFORMING THE NMDS

# m4 <- metaMDS(veg_dat_noinv[, 7:82], k = 2, trymax = 100)
# m4
# plot(m4, type = "t")

# quad 114 still an issue

#m5 <- metaMDS(veg_dat_noinv[-c(101), 7:82], k = 2, trymax = 100)
#m5
#plot(m5, type = "t")

# quads 22 and 84 sill an issue

m6 <- metaMDS(veg_dat_noinv[-c(17, 76, 101), 7:82], k = 2, trymax = 100)
m6
plot(m6, type = "t")

### PREPPING SITE-YEAR DATA

# all the necessary variables
com.scores2 <- as.data.frame(scores(m6))
com.scores2$year <- veg_dat_noinv[-c(17, 76, 101), 5]
com.scores2$site <- veg_dat_noinv[-c(17, 76, 101), 1]
com.scores2$area <- veg_dat_noinv[-c(17, 76, 101), 2]
com.scores2$plot <- veg_dat_noinv[-c(17, 76, 101), 3]
com.scores2$site.year <- as.factor(paste(com.scores2$site, com.scores2$year, sep = ""))
com.scores2$area.year <- as.factor(paste(com.scores2$area, com.scores2$year, sep = ""))
com.scores2$plot.year <- as.factor(paste(com.scores2$plot, com.scores2$year, sep = ""))

#new datasets by site
com.scores.TG2 <- com.scores2[which(com.scores2$site == "TG"), ]
com.scores.BM2 <- com.scores2[which(com.scores2$site == "BM"), ]
com.scores.GL2 <- com.scores2[which(com.scores2$site == "GL"), ]

# The following code creates the centroids and the standard deviation for data points averaged 
# by site and year. 

NMDS1.site.year.mean <- aggregate(NMDS1 ~ site.year, com.scores2, mean) # x coordinate of centroid
NMDS1.site.year.sd <- aggregate(NMDS1 ~ site.year, com.scores2, sd) # x error bar
NMDS2.site.year.mean <- aggregate(NMDS2 ~ site.year, com.scores2, mean) # y coordinate of centroid
NMDS2.site.year.sd <- aggregate(NMDS2 ~ site.year, com.scores2, sd) # y error bar
NMDS.site.year2 <- cbind.data.frame(NMDS1.site.year.mean, NMDS1.site.year.sd$NMDS1,
                                   NMDS2.site.year.mean$NMDS2, NMDS2.site.year.sd$NMDS2)

NMDS.site.year2 <- plyr::rename(NMDS.site.year2, c("site.year" = "site.year", "NMDS1" = "NMDS1.mean",
                                                 "NMDS1.site.year.sd$NMDS1" = "NMDS1.sd",
                                                 "NMDS2.site.year.mean$NMDS2" = "NMDS2.mean",  
                                                 "NMDS2.site.year.sd$NMDS2" = "NMDS2.sd"))
NMDS.site.year2$site <- c("BM", "BM", "GL", "GL", "TG", "TG")
NMDS.site.year2$year <- c(1, 3, 1, 3, 1, 3)

### SITE-YEAR GRAPH

ggplot() + geom_point(data = com.scores2, aes(x = NMDS1, y = NMDS2, color = site, shape = as.factor(year)), alpha = 0.5) +
  geom_point(data = NMDS.site.year2, aes(x = NMDS1.mean, y = NMDS2.mean, color = site, shape = as.factor(year)), size = 4) + 
  geom_errorbar(data = NMDS.site.year2, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = NMDS.site.year2, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +           
  scale_color_manual(name = "Site", values = c("BM" = "royalblue4", "GL" = "goldenrod", "TG" = "green4")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17)) +
  theme_classic(base_size = 12) +
  #scale_x_continuous(limits = c(-1, 1)) +
  #scale_y_continuous(limits = c(-.75, .75)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

ggsave("NMDS by Site and Year Non-Invasives.pdf")

### PREPPING AREA-PLOT DATA

# The following code creates the centroids and the standard deviation for data points averaged 
# by area or plot and year and site. 

# For BM:
# By Area: 

BM.NMDS1.area.mean <- aggregate(NMDS1 ~ area.year, com.scores.BM2, mean)
BM.NMDS1.area.sd <- aggregate(NMDS1 ~ area.year, com.scores.BM2, sd)
BM.NMDS2.area.mean <- aggregate(NMDS2 ~ area.year, com.scores.BM2, mean)
BM.NMDS2.area.sd <- aggregate(NMDS2 ~ area.year, com.scores.BM2, sd)
BM.NMDS.area2 <- cbind.data.frame(BM.NMDS1.area.mean, BM.NMDS1.area.sd$NMDS1,
                                 BM.NMDS2.area.mean$NMDS2, BM.NMDS2.area.sd$NMDS2)

BM.NMDS.area2 <- plyr::rename(BM.NMDS.area2, c("area.year" = "area.year", 
                                             "NMDS1" = "NMDS1.mean", 
                                             "BM.NMDS1.area.sd$NMDS1" = "NMDS1.sd", 
                                             "BM.NMDS2.area.mean$NMDS2" = "NMDS2.mean", 
                                             "BM.NMDS2.area.sd$NMDS2" = "NMDS2.sd"))
BM.NMDS.area2$area <- c("M", "M", "R", "R")
BM.NMDS.area2$year <- c(1, 3, 1, 3)
BM.NMDS.area2$level <- rep("Area", dim(BM.NMDS.area)[[1]])
BM.NMDS.area2$site <- rep("BM", dim(BM.NMDS.area)[[1]])

# By plot:
BM.NMDS1.plot.mean <- aggregate(NMDS1 ~ plot.year, com.scores.BM2, mean)
BM.NMDS1.plot.sd <- aggregate(NMDS1 ~ plot.year, com.scores.BM2, sd)
BM.NMDS2.plot.mean <- aggregate(NMDS2 ~ plot.year, com.scores.BM2, mean)
BM.NMDS2.plot.sd <- aggregate(NMDS2 ~ plot.year, com.scores.BM2, sd)
BM.NMDS.plot2 <- cbind.data.frame(BM.NMDS1.plot.mean, BM.NMDS1.plot.sd$NMDS1,
                                 BM.NMDS2.plot.mean$NMDS2, BM.NMDS2.plot.sd$NMDS2)

BM.NMDS.plot2 <- plyr::rename(BM.NMDS.plot2, c("plot.year" = "plot.year", 
                                             "NMDS1" = "NMDS1.mean", 
                                             "BM.NMDS1.plot.sd$NMDS1" = "NMDS1.sd", 
                                             "BM.NMDS2.plot.mean$NMDS2" = "NMDS2.mean", 
                                             "BM.NMDS2.plot.sd$NMDS2" = "NMDS2.sd"))
BM.NMDS.plot2$plot <- c("F", "F", "N", "N", "O", "O")
BM.NMDS.plot2$year <- c(1, 3, 1, 3, 1, 3)
BM.NMDS.plot2$level<-rep("Plot", dim(BM.NMDS.plot)[[1]])
BM.NMDS.plot2$site<-rep("BM", dim(BM.NMDS.plot)[[1]])

# For GL:
# By area:
GL.NMDS1.area.mean <- aggregate(NMDS1 ~ area.year, com.scores.GL2, mean)
GL.NMDS1.area.sd <- aggregate(NMDS1 ~ area.year, com.scores.GL2, sd)
GL.NMDS2.area.mean <- aggregate(NMDS2 ~ area.year, com.scores.GL2, mean)
GL.NMDS2.area.sd <- aggregate(NMDS2 ~ area.year, com.scores.GL2, sd)
GL.NMDS.area2 <- cbind.data.frame(GL.NMDS1.area.mean, GL.NMDS1.area.sd$NMDS1,
                                 GL.NMDS2.area.mean$NMDS2, GL.NMDS2.area.sd$NMDS2)

GL.NMDS.area2 <- plyr::rename(GL.NMDS.area2, c("area.year" = "area.year", 
                                             "NMDS1" = "NMDS1.mean", 
                                             "GL.NMDS1.area.sd$NMDS1" = "NMDS1.sd", 
                                             "GL.NMDS2.area.mean$NMDS2" = "NMDS2.mean", 
                                             "GL.NMDS2.area.sd$NMDS2" = "NMDS2.sd"))
GL.NMDS.area2$area <- c("M", "M", "R", "R")
GL.NMDS.area2$year <- c(1, 3, 1, 3)
GL.NMDS.area2$level<-rep("Area", dim(GL.NMDS.area)[[1]])
GL.NMDS.area2$site<-rep("GL", dim(GL.NMDS.area)[[1]])

# By plot:
GL.NMDS1.plot.mean <- aggregate(NMDS1 ~ plot.year, com.scores.GL2, mean)
GL.NMDS1.plot.sd <- aggregate(NMDS1 ~ plot.year, com.scores.GL2, sd)
GL.NMDS2.plot.mean <- aggregate(NMDS2 ~ plot.year, com.scores.GL2, mean)
GL.NMDS2.plot.sd <- aggregate(NMDS2 ~ plot.year, com.scores.GL2, sd)
GL.NMDS.plot2 <- cbind.data.frame(GL.NMDS1.plot.mean, GL.NMDS1.plot.sd$NMDS1,
                                 GL.NMDS2.plot.mean$NMDS2, GL.NMDS2.plot.sd$NMDS2)

GL.NMDS.plot2 <- plyr::rename(GL.NMDS.plot2, c("plot.year" = "plot.year", 
                                             "NMDS1" = "NMDS1.mean", 
                                             "GL.NMDS1.plot.sd$NMDS1" = "NMDS1.sd", 
                                             "GL.NMDS2.plot.mean$NMDS2" = "NMDS2.mean", 
                                             "GL.NMDS2.plot.sd$NMDS2" = "NMDS2.sd"))
GL.NMDS.plot2$plot <- c("F", "F", "N", "N", "O", "O")
GL.NMDS.plot2$year <- c(1, 3, 1, 3, 1, 3)
GL.NMDS.plot2$level<-rep("Plot", dim(GL.NMDS.plot)[[1]])
GL.NMDS.plot2$site<-rep("GL", dim(GL.NMDS.plot)[[1]])

# For TG
# By area:
TG.NMDS1.area.mean <- aggregate(NMDS1 ~ area.year, com.scores.TG2, mean)
TG.NMDS1.area.sd <- aggregate(NMDS1 ~ area.year, com.scores.TG2, sd)
TG.NMDS2.area.mean <- aggregate(NMDS2 ~ area.year, com.scores.TG2, mean)
TG.NMDS2.area.sd <- aggregate(NMDS2 ~ area.year, com.scores.TG2, sd)
TG.NMDS.area2 <- cbind.data.frame(TG.NMDS1.area.mean, TG.NMDS1.area.sd$NMDS1,
                                 TG.NMDS2.area.mean$NMDS2, TG.NMDS2.area.sd$NMDS2)

TG.NMDS.area2 <- plyr::rename(TG.NMDS.area2, c("area.year" = "area.year", 
                                             "NMDS1" = "NMDS1.mean", 
                                             "TG.NMDS1.area.sd$NMDS1" = "NMDS1.sd", 
                                             "TG.NMDS2.area.mean$NMDS2" = "NMDS2.mean", 
                                             "TG.NMDS2.area.sd$NMDS2" = "NMDS2.sd"))
TG.NMDS.area2$area <- c("M", "M", "R", "R")
TG.NMDS.area2$year <- c(1, 3, 1, 3)
TG.NMDS.area2$level<-rep("Area", dim(TG.NMDS.area)[[1]])
TG.NMDS.area2$site<-rep("TG", dim(TG.NMDS.area)[[1]])

# By plot: 
TG.NMDS1.plot.mean <- aggregate(NMDS1 ~ plot.year, com.scores.TG2, mean)
TG.NMDS1.plot.sd <- aggregate(NMDS1 ~ plot.year, com.scores.TG2, sd)
TG.NMDS2.plot.mean <- aggregate(NMDS2 ~ plot.year, com.scores.TG2, mean)
TG.NMDS2.plot.sd <- aggregate(NMDS2 ~ plot.year, com.scores.TG2, sd)
TG.NMDS.plot2 <- cbind.data.frame(TG.NMDS1.plot.mean, TG.NMDS1.plot.sd$NMDS1,
                                 TG.NMDS2.plot.mean$NMDS2, TG.NMDS2.plot.sd$NMDS2)

TG.NMDS.plot2 <- plyr::rename(TG.NMDS.plot2, c("plot.year" = "plot.year", 
                                             "NMDS1" = "NMDS1.mean", 
                                             "TG.NMDS1.plot.sd$NMDS1" = "NMDS1.sd", 
                                             "TG.NMDS2.plot.mean$NMDS2" = "NMDS2.mean", 
                                             "TG.NMDS2.plot.sd$NMDS2" = "NMDS2.sd"))
TG.NMDS.plot2$plot <- c("F", "F", "N", "N", "O", "O")
TG.NMDS.plot2$year <- c(1, 3, 1, 3, 1, 3)
TG.NMDS.plot2$level<-rep("Plot", dim(TG.NMDS.plot)[[1]])
TG.NMDS.plot2$site<-rep("TG", dim(TG.NMDS.plot)[[1]])

### AREA-PLOT GRAPH

BM.A2 <- ggplot() + geom_point(data = com.scores.BM2, aes(x = NMDS1, y = NMDS2, color = area, shape = as.factor(year)), alpha = 0.4) +
  geom_point(data = BM.NMDS.area2, aes(x = NMDS1.mean, y = NMDS2.mean, color = area, shape = as.factor(year)), size = 4, alpha = 1) +
  geom_errorbar(data = BM.NMDS.area2, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = BM.NMDS.area2, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +
  scale_color_manual(name = "Area", labels = c("M" = "Management", "R" = "Reference"), 
                     values = c("M" = "gray53", "R" = "darkorange1")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17), labels = c("1" = "2018", "3" = "2020")) +
  theme(axis.text.y   = element_text(size=12),
        axis.text.x   = element_text(size=12),
        axis.title.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(color="black", fill="white"), 
        legend.key = element_rect(colour = NA, fill = NA)) + 
  xlab(element_blank()) + 
  scale_x_continuous(limits = c(-.7, .7), breaks = c(-.5, -.25, 0, .25, .5)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -.5, 0, .5, 1)) +
  facet_grid(. ~ level)

BM.P2 <- ggplot() + geom_point(data = com.scores.BM2, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.4) +
  geom_point(data = BM.NMDS.plot2, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4, alpha = 1) +
  geom_errorbar(data = BM.NMDS.plot2, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = BM.NMDS.plot2, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +
  scale_color_manual(name = "Plot", labels = c("F" = "Fenced", "N" = "No V. rossicum", "O" = "Open"), 
                     values = c("F" = "royalblue4", "N" = "goldenrod", "O" = "green4")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17), labels = c("1" = "2018", "3" = "2020")) +
  theme(axis.text.y   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x   = element_text(size=12),
        axis.title.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(color="black", fill="white"), 
        legend.key = element_rect(colour = NA, fill = NA)) + 
  xlab(element_blank()) + ylab(element_blank()) + 
  scale_x_continuous(limits = c(-.7, .7), breaks = c(-.5, -.25, 0, .25, .5)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -.5, 0, .5, 1)) +
  facet_grid(site ~ level)

GL.A2 <- ggplot() + geom_point(data = com.scores.GL2, aes(x = NMDS1, y = NMDS2, color = area, shape = as.factor(year)), alpha = 0.4) +
  geom_point(data = GL.NMDS.area2, aes(x = NMDS1.mean, y = NMDS2.mean, color = area, shape = as.factor(year)), size = 4, alpha = 1) +
  geom_errorbar(data = GL.NMDS.area2, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = GL.NMDS.area2, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +
  scale_color_manual(name = "Area", labels = c("M" = "Management", "R" = "Reference"), 
                     values = c("M" = "gray53", "R" = "darkorange1")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17), labels = c("1" = "2018", "3" = "2020")) +
  theme(axis.text.y   = element_text(size=12),
        axis.text.x   = element_text(size=12),
        axis.title.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(color="black", fill="white"), 
        legend.key = element_rect(colour = NA, fill = NA)) + 
  xlab(element_blank()) + 
  scale_x_continuous(limits = c(-.35, .8), breaks = c(-.25, 0, .25, .5, .75)) +
  scale_y_continuous(limits = c(-.4, 1), breaks = c(-.2, .1, .4, .7, 1))

GL.P2 <- ggplot() + geom_point(data = com.scores.GL2, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.4) +
  geom_point(data = GL.NMDS.plot2, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4, alpha = 1) +
  geom_errorbar(data = GL.NMDS.plot2, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = GL.NMDS.plot2, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +
  scale_color_manual(name = "Plot", labels = c("F" = "Fenced", "N" = "No V. rossicum", "O" = "Open"), 
                     values = c("F" = "royalblue4", "N" = "goldenrod", "O" = "green4")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17), labels = c("1" = "2018", "3" = "2020")) +
  theme(axis.text.y   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x   = element_text(size=12),
        axis.title.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(color="black", fill="white"), 
        legend.key = element_rect(colour = NA, fill = NA)) + 
  xlab(element_blank()) + ylab(element_blank()) + 
  scale_x_continuous(limits = c(-.35, .8), breaks = c(-.25, 0, .25, .5, .75)) +
  scale_y_continuous(limits = c(-.4, 1), breaks = c(-.2, .1, .4, .7, 1)) +
  facet_grid(site ~ .)

TG.A2 <- ggplot() + geom_point(data = com.scores.TG2, aes(x = NMDS1, y = NMDS2, color = area, shape = as.factor(year)), alpha = 0.4) +
  geom_point(data = TG.NMDS.area2, aes(x = NMDS1.mean, y = NMDS2.mean, color = area, shape = as.factor(year)), size = 4, alpha = 1) +
  geom_errorbar(data = TG.NMDS.area2, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = TG.NMDS.area2, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +
  scale_color_manual(name = "Area", labels = c("M" = "Management", "R" = "Reference"), 
                     values = c("M" = "gray53", "R" = "darkorange1")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17), labels = c("1" = "2018", "3" = "2020")) +
  theme(axis.text.y   = element_text(size=12),
        axis.text.x   = element_text(size=12),
        axis.title.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(color="black", fill="white"), 
        legend.key = element_rect(colour = NA, fill = NA)) + 
  scale_x_continuous(limits = c(-1.7, 1), breaks = c(-1.4, -.8, -.2, .4, 1)) +
  scale_y_continuous(limits = c(-.8, .6), breaks = c(-.6, -.3, 0, .3, .6))

TG.P2 <- ggplot() + geom_point(data = com.scores.TG2, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.4) +
  geom_point(data = TG.NMDS.plot2, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4, alpha = 1) +
  geom_errorbar(data = TG.NMDS.plot2, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = TG.NMDS.plot2, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +
  scale_color_manual(name = "Plot", labels = c("F" = "Fenced", "N" = "No V. rossicum", "O" = "Open"), 
                     values = c("F" = "royalblue4", "N" = "goldenrod", "O" = "green4")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17), labels = c("1" = "2018", "3" = "2020")) +
  theme(axis.text.y   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x   = element_text(size=12),
        axis.title.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        strip.background = element_rect(color="black", fill="white"),
        legend.key = element_rect(colour = NA, fill = NA)) + 
  ylab(element_blank()) +
  scale_x_continuous(limits = c(-1.7, 1), breaks = c(-1.4, -.8, -.2, .4, 1)) +
  scale_y_continuous(limits = c(-.8, .6), breaks = c(-.6, -.3, 0, .3, .6)) +
  facet_grid(site ~ .)


(BM.A2 | BM.P2) / (GL.A2 | GL.P2) / (TG.A2 | TG.P2) + plot_layout(guides="collect")

ggsave("NMDS by Area and Plot Non-Invasives.pdf")











##### III. ONLY NATIVE PLANTS

### PREPPING DATA FOR NMDS

# take out nonative species

veg_dat_nat <- veg_dat_noinv[, -c(12, 21, 29, 31, 36, 39, 42, 43, 49, 57, 61, 64, 70, 71, 76:84)]

deleted_rows3 <- which(rowSums(veg_dat_nat[, 7:61]) == 0) # Taking out rows with 0% total cover

veg_dat_nat <- veg_dat_nat[-deleted_rows3,] 

### PERFORMING THE NMDS

#m7 <- metaMDS(veg_dat_nat[, 7:61], k = 2, trymax = 100)
#m7
#plot(m7, type = "t")

# quad 134 is a bit of an outlier

m8 <- metaMDS(veg_dat_nat[-c(108), 7:61], k = 2, trymax = 100)
m8
plot(m8, type = "t")


### PREPPING SITE-YEAR DATA

# all the necessary variables
com.scores3 <- as.data.frame(scores(m8))
com.scores3$year <- veg_dat_nat[-c(108), 5]
com.scores3$site <- veg_dat_nat[-c(108), 1]
com.scores3$area <- veg_dat_nat[-c(108), 2]
com.scores3$plot <- veg_dat_nat[-c(108), 3]
com.scores3$site.year <- as.factor(paste(com.scores3$site, com.scores3$year, sep = ""))
com.scores3$area.year <- as.factor(paste(com.scores3$area, com.scores3$year, sep = ""))
com.scores3$plot.year <- as.factor(paste(com.scores3$plot, com.scores3$year, sep = ""))

#new datasets by site
com.scores.TG3 <- com.scores3[which(com.scores3$site == "TG"), ]
com.scores.BM3 <- com.scores3[which(com.scores3$site == "BM"), ]
com.scores.GL3 <- com.scores3[which(com.scores3$site == "GL"), ]

# The following code creates the centroids and the standard deviation for data points averaged 
# by site and year. 

NMDS1.site.year.mean <- aggregate(NMDS1 ~ site.year, com.scores3, mean) # x coordinate of centroid
NMDS1.site.year.sd <- aggregate(NMDS1 ~ site.year, com.scores3, sd) # x error bar
NMDS2.site.year.mean <- aggregate(NMDS2 ~ site.year, com.scores3, mean) # y coordinate of centroid
NMDS2.site.year.sd <- aggregate(NMDS2 ~ site.year, com.scores3, sd) # y error bar
NMDS.site.year3 <- cbind.data.frame(NMDS1.site.year.mean, NMDS1.site.year.sd$NMDS1,
                                    NMDS2.site.year.mean$NMDS2, NMDS2.site.year.sd$NMDS2)

NMDS.site.year3 <- plyr::rename(NMDS.site.year3, c("site.year" = "site.year", "NMDS1" = "NMDS1.mean",
                                                   "NMDS1.site.year.sd$NMDS1" = "NMDS1.sd",
                                                   "NMDS2.site.year.mean$NMDS2" = "NMDS2.mean",  
                                                   "NMDS2.site.year.sd$NMDS2" = "NMDS2.sd"))
NMDS.site.year3$site <- c("BM", "BM", "GL", "GL", "TG", "TG")
NMDS.site.year3$year <- c(1, 3, 1, 3, 1, 3)

### SITE-YEAR GRAPH

ggplot() + geom_point(data = com.scores3, aes(x = NMDS1, y = NMDS2, color = site, shape = as.factor(year)), alpha = 0.5) +
  geom_point(data = NMDS.site.year3, aes(x = NMDS1.mean, y = NMDS2.mean, color = site, shape = as.factor(year)), size = 4) + 
  geom_errorbar(data = NMDS.site.year3, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = NMDS.site.year3, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +           
  scale_color_manual(name = "Site", values = c("BM" = "royalblue4", "GL" = "goldenrod", "TG" = "green4")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17)) +
  theme_classic(base_size = 12) +
  #scale_x_continuous(limits = c(-1, 1)) +
  #scale_y_continuous(limits = c(-.75, .75)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

ggsave("NMDS by Site and Year Natives.pdf")

### PREPPING AREA-PLOT DATA

# The following code creates the centroids and the standard deviation for data points averaged 
# by area or plot and year and site. 

# For BM:
# By Area: 

BM.NMDS1.area.mean <- aggregate(NMDS1 ~ area.year, com.scores.BM3, mean)
BM.NMDS1.area.sd <- aggregate(NMDS1 ~ area.year, com.scores.BM3, sd)
BM.NMDS2.area.mean <- aggregate(NMDS2 ~ area.year, com.scores.BM3, mean)
BM.NMDS2.area.sd <- aggregate(NMDS2 ~ area.year, com.scores.BM3, sd)
BM.NMDS.area3 <- cbind.data.frame(BM.NMDS1.area.mean, BM.NMDS1.area.sd$NMDS1,
                                  BM.NMDS2.area.mean$NMDS2, BM.NMDS2.area.sd$NMDS2)

BM.NMDS.area3 <- plyr::rename(BM.NMDS.area3, c("area.year" = "area.year", 
                                               "NMDS1" = "NMDS1.mean", 
                                               "BM.NMDS1.area.sd$NMDS1" = "NMDS1.sd", 
                                               "BM.NMDS2.area.mean$NMDS2" = "NMDS2.mean", 
                                               "BM.NMDS2.area.sd$NMDS2" = "NMDS2.sd"))
BM.NMDS.area3$area <- c("M", "M", "R", "R")
BM.NMDS.area3$year <- c(1, 3, 1, 3)
BM.NMDS.area3$level <- rep("Area", dim(BM.NMDS.area)[[1]])
BM.NMDS.area3$site <- rep("BM", dim(BM.NMDS.area)[[1]])

# By plot:
BM.NMDS1.plot.mean <- aggregate(NMDS1 ~ plot.year, com.scores.BM3, mean)
BM.NMDS1.plot.sd <- aggregate(NMDS1 ~ plot.year, com.scores.BM3, sd)
BM.NMDS2.plot.mean <- aggregate(NMDS2 ~ plot.year, com.scores.BM3, mean)
BM.NMDS2.plot.sd <- aggregate(NMDS2 ~ plot.year, com.scores.BM3, sd)
BM.NMDS.plot3 <- cbind.data.frame(BM.NMDS1.plot.mean, BM.NMDS1.plot.sd$NMDS1,
                                  BM.NMDS2.plot.mean$NMDS2, BM.NMDS2.plot.sd$NMDS2)

BM.NMDS.plot3 <- plyr::rename(BM.NMDS.plot3, c("plot.year" = "plot.year", 
                                               "NMDS1" = "NMDS1.mean", 
                                               "BM.NMDS1.plot.sd$NMDS1" = "NMDS1.sd", 
                                               "BM.NMDS2.plot.mean$NMDS2" = "NMDS2.mean", 
                                               "BM.NMDS2.plot.sd$NMDS2" = "NMDS2.sd"))
BM.NMDS.plot3$plot <- c("F", "F", "N", "N", "O", "O")
BM.NMDS.plot3$year <- c(1, 3, 1, 3, 1, 3)
BM.NMDS.plot3$level<-rep("Plot", dim(BM.NMDS.plot)[[1]])
BM.NMDS.plot3$site<-rep("BM", dim(BM.NMDS.plot)[[1]])

# For GL:
# By area:
GL.NMDS1.area.mean <- aggregate(NMDS1 ~ area.year, com.scores.GL3, mean)
GL.NMDS1.area.sd <- aggregate(NMDS1 ~ area.year, com.scores.GL3, sd)
GL.NMDS2.area.mean <- aggregate(NMDS2 ~ area.year, com.scores.GL3, mean)
GL.NMDS2.area.sd <- aggregate(NMDS2 ~ area.year, com.scores.GL3, sd)
GL.NMDS.area3 <- cbind.data.frame(GL.NMDS1.area.mean, GL.NMDS1.area.sd$NMDS1,
                                  GL.NMDS2.area.mean$NMDS2, GL.NMDS2.area.sd$NMDS2)

GL.NMDS.area3 <- plyr::rename(GL.NMDS.area3, c("area.year" = "area.year", 
                                               "NMDS1" = "NMDS1.mean", 
                                               "GL.NMDS1.area.sd$NMDS1" = "NMDS1.sd", 
                                               "GL.NMDS2.area.mean$NMDS2" = "NMDS2.mean", 
                                               "GL.NMDS2.area.sd$NMDS2" = "NMDS2.sd"))
GL.NMDS.area3$area <- c("M", "M", "R", "R")
GL.NMDS.area3$year <- c(1, 3, 1, 3)
GL.NMDS.area3$level<-rep("Area", dim(GL.NMDS.area)[[1]])
GL.NMDS.area3$site<-rep("GL", dim(GL.NMDS.area)[[1]])

# By plot:
GL.NMDS1.plot.mean <- aggregate(NMDS1 ~ plot.year, com.scores.GL3, mean)
GL.NMDS1.plot.sd <- aggregate(NMDS1 ~ plot.year, com.scores.GL3, sd)
GL.NMDS2.plot.mean <- aggregate(NMDS2 ~ plot.year, com.scores.GL3, mean)
GL.NMDS2.plot.sd <- aggregate(NMDS2 ~ plot.year, com.scores.GL3, sd)
GL.NMDS.plot3 <- cbind.data.frame(GL.NMDS1.plot.mean, GL.NMDS1.plot.sd$NMDS1,
                                  GL.NMDS2.plot.mean$NMDS2, GL.NMDS2.plot.sd$NMDS2)

GL.NMDS.plot3 <- plyr::rename(GL.NMDS.plot3, c("plot.year" = "plot.year", 
                                               "NMDS1" = "NMDS1.mean", 
                                               "GL.NMDS1.plot.sd$NMDS1" = "NMDS1.sd", 
                                               "GL.NMDS2.plot.mean$NMDS2" = "NMDS2.mean", 
                                               "GL.NMDS2.plot.sd$NMDS2" = "NMDS2.sd"))
GL.NMDS.plot3$plot <- c("F", "F", "N", "N", "O", "O")
GL.NMDS.plot3$year <- c(1, 3, 1, 3, 1, 3)
GL.NMDS.plot3$level<-rep("Plot", dim(GL.NMDS.plot)[[1]])
GL.NMDS.plot3$site<-rep("GL", dim(GL.NMDS.plot)[[1]])

# For TG
# By area:
TG.NMDS1.area.mean <- aggregate(NMDS1 ~ area.year, com.scores.TG3, mean)
TG.NMDS1.area.sd <- aggregate(NMDS1 ~ area.year, com.scores.TG3, sd)
TG.NMDS2.area.mean <- aggregate(NMDS2 ~ area.year, com.scores.TG3, mean)
TG.NMDS2.area.sd <- aggregate(NMDS2 ~ area.year, com.scores.TG3, sd)
TG.NMDS.area3 <- cbind.data.frame(TG.NMDS1.area.mean, TG.NMDS1.area.sd$NMDS1,
                                  TG.NMDS2.area.mean$NMDS2, TG.NMDS2.area.sd$NMDS2)

TG.NMDS.area3 <- plyr::rename(TG.NMDS.area3, c("area.year" = "area.year", 
                                               "NMDS1" = "NMDS1.mean", 
                                               "TG.NMDS1.area.sd$NMDS1" = "NMDS1.sd", 
                                               "TG.NMDS2.area.mean$NMDS2" = "NMDS2.mean", 
                                               "TG.NMDS2.area.sd$NMDS2" = "NMDS2.sd"))
TG.NMDS.area3$area <- c("M", "M", "R", "R")
TG.NMDS.area3$year <- c(1, 3, 1, 3)
TG.NMDS.area3$level<-rep("Area", dim(TG.NMDS.area)[[1]])
TG.NMDS.area3$site<-rep("TG", dim(TG.NMDS.area)[[1]])

# By plot: 
TG.NMDS1.plot.mean <- aggregate(NMDS1 ~ plot.year, com.scores.TG3, mean)
TG.NMDS1.plot.sd <- aggregate(NMDS1 ~ plot.year, com.scores.TG3, sd)
TG.NMDS2.plot.mean <- aggregate(NMDS2 ~ plot.year, com.scores.TG3, mean)
TG.NMDS2.plot.sd <- aggregate(NMDS2 ~ plot.year, com.scores.TG3, sd)
TG.NMDS.plot3 <- cbind.data.frame(TG.NMDS1.plot.mean, TG.NMDS1.plot.sd$NMDS1,
                                  TG.NMDS2.plot.mean$NMDS2, TG.NMDS2.plot.sd$NMDS2)

TG.NMDS.plot3 <- plyr::rename(TG.NMDS.plot3, c("plot.year" = "plot.year", 
                                               "NMDS1" = "NMDS1.mean", 
                                               "TG.NMDS1.plot.sd$NMDS1" = "NMDS1.sd", 
                                               "TG.NMDS2.plot.mean$NMDS2" = "NMDS2.mean", 
                                               "TG.NMDS2.plot.sd$NMDS2" = "NMDS2.sd"))
TG.NMDS.plot3$plot <- c("F", "F", "N", "N", "O", "O")
TG.NMDS.plot3$year <- c(1, 3, 1, 3, 1, 3)
TG.NMDS.plot3$level<-rep("Plot", dim(TG.NMDS.plot)[[1]])
TG.NMDS.plot3$site<-rep("TG", dim(TG.NMDS.plot)[[1]])

### AREA-PLOT GRAPH

BM.A3 <- ggplot() + geom_point(data = com.scores.BM3, aes(x = NMDS1, y = NMDS2, color = area, shape = as.factor(year)), alpha = 0.4) +
  geom_point(data = BM.NMDS.area3, aes(x = NMDS1.mean, y = NMDS2.mean, color = area, shape = as.factor(year)), size = 4, alpha = 1) +
  geom_errorbar(data = BM.NMDS.area3, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = BM.NMDS.area3, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +
  scale_color_manual(name = "Area", labels = c("M" = "Management", "R" = "Reference"), 
                     values = c("M" = "gray53", "R" = "darkorange1")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17), labels = c("1" = "2018", "3" = "2020")) +
  theme(axis.text.y   = element_text(size=12),
        axis.text.x   = element_text(size=12),
        axis.title.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(color="black", fill="white"), 
        legend.key = element_rect(colour = NA, fill = NA)) + 
  xlab(element_blank()) + 
  scale_x_continuous(limits = c(-.7, .7), breaks = c(-.5, -.25, 0, .25, .5)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -.5, 0, .5, 1)) +
  facet_grid(. ~ level)

BM.P3 <- ggplot() + geom_point(data = com.scores.BM3, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.4) +
  geom_point(data = BM.NMDS.plot3, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4, alpha = 1) +
  geom_errorbar(data = BM.NMDS.plot3, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = BM.NMDS.plot3, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +
  scale_color_manual(name = "Plot", labels = c("F" = "Fenced", "N" = "No V. rossicum", "O" = "Open"), 
                     values = c("F" = "royalblue4", "N" = "goldenrod", "O" = "green4")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17), labels = c("1" = "2018", "3" = "2020")) +
  theme(axis.text.y   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x   = element_text(size=12),
        axis.title.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(color="black", fill="white"), 
        legend.key = element_rect(colour = NA, fill = NA)) + 
  xlab(element_blank()) + ylab(element_blank()) + 
  scale_x_continuous(limits = c(-.7, .7), breaks = c(-.5, -.25, 0, .25, .5)) +
  scale_y_continuous(limits = c(-1, 1), breaks = c(-1, -.5, 0, .5, 1)) +
  facet_grid(site ~ level)

GL.A3 <- ggplot() + geom_point(data = com.scores.GL3, aes(x = NMDS1, y = NMDS2, color = area, shape = as.factor(year)), alpha = 0.4) +
  geom_point(data = GL.NMDS.area3, aes(x = NMDS1.mean, y = NMDS2.mean, color = area, shape = as.factor(year)), size = 4, alpha = 1) +
  geom_errorbar(data = GL.NMDS.area3, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = GL.NMDS.area3, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +
  scale_color_manual(name = "Area", labels = c("M" = "Management", "R" = "Reference"), 
                     values = c("M" = "gray53", "R" = "darkorange1")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17), labels = c("1" = "2018", "3" = "2020")) +
  theme(axis.text.y   = element_text(size=12),
        axis.text.x   = element_text(size=12),
        axis.title.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(color="black", fill="white"), 
        legend.key = element_rect(colour = NA, fill = NA)) + 
  xlab(element_blank()) + 
  scale_x_continuous(limits = c(-.35, .8), breaks = c(-.25, 0, .25, .5, .75)) +
  scale_y_continuous(limits = c(-.4, 1), breaks = c(-.2, .1, .4, .7, 1))

GL.P3 <- ggplot() + geom_point(data = com.scores.GL3, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.4) +
  geom_point(data = GL.NMDS.plot3, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4, alpha = 1) +
  geom_errorbar(data = GL.NMDS.plot3, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = GL.NMDS.plot3, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +
  scale_color_manual(name = "Plot", labels = c("F" = "Fenced", "N" = "No V. rossicum", "O" = "Open"), 
                     values = c("F" = "royalblue4", "N" = "goldenrod", "O" = "green4")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17), labels = c("1" = "2018", "3" = "2020")) +
  theme(axis.text.y   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x   = element_text(size=12),
        axis.title.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(color="black", fill="white"), 
        legend.key = element_rect(colour = NA, fill = NA)) + 
  xlab(element_blank()) + ylab(element_blank()) + 
  scale_x_continuous(limits = c(-.35, .8), breaks = c(-.25, 0, .25, .5, .75)) +
  scale_y_continuous(limits = c(-.4, 1), breaks = c(-.2, .1, .4, .7, 1)) +
  facet_grid(site ~ .)

TG.A3 <- ggplot() + geom_point(data = com.scores.TG3, aes(x = NMDS1, y = NMDS2, color = area, shape = as.factor(year)), alpha = 0.4) +
  geom_point(data = TG.NMDS.area3, aes(x = NMDS1.mean, y = NMDS2.mean, color = area, shape = as.factor(year)), size = 4, alpha = 1) +
  geom_errorbar(data = TG.NMDS.area3, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = TG.NMDS.area3, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +
  scale_color_manual(name = "Area", labels = c("M" = "Management", "R" = "Reference"), 
                     values = c("M" = "gray53", "R" = "darkorange1")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17), labels = c("1" = "2018", "3" = "2020")) +
  theme(axis.text.y   = element_text(size=12),
        axis.text.x   = element_text(size=12),
        axis.title.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1),
        strip.background = element_rect(color="black", fill="white"), 
        legend.key = element_rect(colour = NA, fill = NA)) + 
  scale_x_continuous(limits = c(-1.7, 1), breaks = c(-1.4, -.8, -.2, .4, 1)) +
  scale_y_continuous(limits = c(-.8, .6), breaks = c(-.6, -.3, 0, .3, .6))

TG.P3 <- ggplot() + geom_point(data = com.scores.TG3, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.4) +
  geom_point(data = TG.NMDS.plot3, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4, alpha = 1) +
  geom_errorbar(data = TG.NMDS.plot3, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = TG.NMDS.plot3, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +
  scale_color_manual(name = "Plot", labels = c("F" = "Fenced", "N" = "No V. rossicum", "O" = "Open"), 
                     values = c("F" = "royalblue4", "N" = "goldenrod", "O" = "green4")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17), labels = c("1" = "2018", "3" = "2020")) +
  theme(axis.text.y   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.x   = element_text(size=12),
        axis.title.y  = element_text(size=12),
        axis.title.x  = element_text(size=12),
        panel.background = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1), 
        strip.background = element_rect(color="black", fill="white"),
        legend.key = element_rect(colour = NA, fill = NA)) + 
  ylab(element_blank()) +
  scale_x_continuous(limits = c(-1.7, 1), breaks = c(-1.4, -.8, -.2, .4, 1)) +
  scale_y_continuous(limits = c(-.8, .6), breaks = c(-.6, -.3, 0, .3, .6)) +
  facet_grid(site ~ .)


(BM.A3 | BM.P3) / (GL.A3 | GL.P3) / (TG.A3 | TG.P3) + plot_layout(guides="collect")

ggsave("NMDS by Area and Plot Natives.pdf")