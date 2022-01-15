
library(tidyverse)
library(vegan)
library(patchwork)

##### I. WHOLE PLANT CIMMUNITY

### SITE-YEAR NMDS

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

ggplot() + geom_point(data = com.scores, aes(x = NMDS1, y = NMDS2, color = site, shape = as.factor(year)), alpha = 0.5) +
  geom_point(data = NMDS.site.year, aes(x = NMDS1.mean, y = NMDS2.mean, color = site, shape = as.factor(year)), size = 4) + 
  geom_errorbar(data = NMDS.site.year, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = NMDS.site.year, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +           
  scale_color_manual(name = "Site", values = c("BM" = "royalblue4", "GL" = "goldenrod", "TG" = "green4")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17)) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_continuous(limits = c(-1, 1)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

### AREA-PLOT-YEAR NMDS

## INVADED DATA [FENCING COMPARISON]

invaded_dat <- veg_dat[veg_dat$plot != "N", ]

which(rowSums(invaded_dat[, 7:89]) == 0) # all rows still contain some veg

# m4 <- metaMDS(invaded_dat[, 7:89], k = 2, trymax = 100)
# m4
# plot(m4, type = "t")

#114

# m5 <- metaMDS(invaded_dat[-c(72), 7:89], k = 2, trymax = 100)
# m5
# plot(m5, type = "t")

#84

# m6 <- metaMDS(invaded_dat[-c(55, 72), 7:89], k = 2, trymax = 100)
# m6
# plot(m6, type = "t")

# 43

# m7 <- metaMDS(invaded_dat[-c(32, 55, 72), 7:89], k = 2, trymax = 100)
# m7
# plot(m7, type = "t")

# 121 and 282

m8 <- metaMDS(invaded_dat[-c(32, 55, 72, 76, 175), 7:89], k = 2, trymax = 100)
m8
plot(m8, type = "t")

I_scores <- as.data.frame(scores(m8))
I_scores$year <- invaded_dat[-c(32, 55, 72, 76, 175), 5]
I_scores$plot <- invaded_dat[-c(32, 55, 72, 76, 175), 3]
I_scores$area <- invaded_dat[-c(32, 55, 72, 76, 175), 2]
I_scores$plot.year <- as.factor(paste(I_scores$plot, I_scores$year, sep = ""))

### MANAGED

I_scores_managed <- I_scores[I_scores$area == "M", ]
I_NMDS1.plot.year.mean <- aggregate(NMDS1 ~ plot.year, I_scores_managed, mean) # x coordinate of centroid
I_NMDS1.plot.year.sd <- aggregate(NMDS1 ~ plot.year, I_scores_managed, sd) # x error bar
I_NMDS2.plot.year.mean <- aggregate(NMDS2 ~ plot.year, I_scores_managed, mean) # y coordinate of centroid
I_NMDS2.plot.year.sd <- aggregate(NMDS2 ~ plot.year, I_scores_managed, sd) # y error bar
I_NMDS.plot.year <- cbind.data.frame(I_NMDS1.plot.year.mean, I_NMDS1.plot.year.sd$NMDS1,
                                     I_NMDS2.plot.year.mean$NMDS2, I_NMDS2.plot.year.sd$NMDS2)

I_NMDS.plot.year <- plyr::rename(I_NMDS.plot.year, c("plot.year" = "plot.year", "NMDS1" = "NMDS1.mean",
                                                     "I_NMDS1.plot.year.sd$NMDS1" = "NMDS1.sd",
                                                     "I_NMDS2.plot.year.mean$NMDS2" = "NMDS2.mean",  
                                                     "I_NMDS2.plot.year.sd$NMDS2" = "NMDS2.sd"))
I_NMDS.plot.year$plot <- c("F", "F", "O", "O")
I_NMDS.plot.year$year <- c(1, 3, 1, 3)

I_scores_managed <- I_scores_managed[-c(39), ]

ggplot() + geom_point(data = I_scores_managed, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.5) +
  geom_point(data = I_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4) + 
  geom_errorbar(data = I_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = I_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +           
  scale_color_manual(name = "Plot", values = c("F" = "green4", "O" = "goldenrod")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17)) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-1.3, 0.9), breaks = seq(-1.3, 0.9, 0.55)) +
  scale_y_continuous(limits = c(-0.8, 0.8), breaks = seq(-0.8, 0.8, 0.4)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

### UNMANAGED

I_scores_reference <- I_scores[I_scores$area == "R", ]
I_NMDS1.plot.year.mean <- aggregate(NMDS1 ~ plot.year, I_scores_reference, mean) # x coordinate of centroid
I_NMDS1.plot.year.sd <- aggregate(NMDS1 ~ plot.year, I_scores_reference, sd) # x error bar
I_NMDS2.plot.year.mean <- aggregate(NMDS2 ~ plot.year, I_scores_reference, mean) # y coordinate of centroid
I_NMDS2.plot.year.sd <- aggregate(NMDS2 ~ plot.year, I_scores_reference, sd) # y error bar
I_NMDS.plot.year <- cbind.data.frame(I_NMDS1.plot.year.mean, I_NMDS1.plot.year.sd$NMDS1,
                                     I_NMDS2.plot.year.mean$NMDS2, I_NMDS2.plot.year.sd$NMDS2)

I_NMDS.plot.year <- plyr::rename(I_NMDS.plot.year, c("plot.year" = "plot.year", "NMDS1" = "NMDS1.mean",
                                                     "I_NMDS1.plot.year.sd$NMDS1" = "NMDS1.sd",
                                                     "I_NMDS2.plot.year.mean$NMDS2" = "NMDS2.mean",  
                                                     "I_NMDS2.plot.year.sd$NMDS2" = "NMDS2.sd"))
I_NMDS.plot.year$plot <- c("F", "F", "O", "O")
I_NMDS.plot.year$year <- c(1, 3, 1, 3)


I_scores_reference <- I_scores_reference[-c(2, 5), ]

ggplot() + geom_point(data = I_scores_reference, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.5) +
  geom_point(data = I_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4) + 
  geom_errorbar(data = I_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = I_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +           
  scale_color_manual(name = "Plot", values = c("F" = "green4", "O" = "goldenrod")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17)) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-0.7, 0.7), breaks = seq(-0.7, 0.7, 0.35)) +
  scale_y_continuous(limits = c(-1.2, 1), breaks = seq(-1.2, 1, 0.55)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))


### OPEN DATA [INVASION COMPARISON]

open_dat <- veg_dat[veg_dat$plot != "F", ]

which(rowSums(open_dat[, 7:89]) == 0) # all rows still contain some veg

# m9 <- metaMDS(open_dat[, 7:89], k = 2, trymax = 100)
# m9
# plot(m9, type = "t")

# 22

# m10 <- metaMDS(open_dat[-c(10), 7:89], k = 2, trymax = 100)
# m10
# plot(m10, type = "t")

# 46 + 23

# m11 <- metaMDS(open_dat[-c(10, 11, 23), 7:89], k = 2, trymax = 100)
# m11
# plot(m11, type = "t")

# 282

m12 <- metaMDS(open_dat[-c(10, 11, 23, 168), 7:89], k = 2, trymax = 100)
m12
plot(m12, type = "t")

O_scores <- as.data.frame(scores(m12))
O_scores$year <- open_dat[-c(10, 11, 23, 168), 5]
O_scores$area <- open_dat[-c(10, 11, 23, 168), 2]
O_scores$plot <- open_dat[-c(10, 11, 23, 168), 3]
O_scores$plot.year <- as.factor(paste(O_scores$plot, O_scores$year, sep = ""))

### MANAGED

O_scores_managed <- O_scores[O_scores$area == "M", ]
O_NMDS1.plot.year.mean <- aggregate(NMDS1 ~ plot.year, O_scores_managed, mean) # x coordinate of centroid
O_NMDS1.plot.year.sd <- aggregate(NMDS1 ~ plot.year, O_scores_managed, sd) # x error bar
O_NMDS2.plot.year.mean <- aggregate(NMDS2 ~ plot.year, O_scores_managed, mean) # y coordinate of centroid
O_NMDS2.plot.year.sd <- aggregate(NMDS2 ~ plot.year, O_scores_managed, sd) # y error bar
O_NMDS.plot.year <- cbind.data.frame(O_NMDS1.plot.year.mean, O_NMDS1.plot.year.sd$NMDS1,
                                     O_NMDS2.plot.year.mean$NMDS2, O_NMDS2.plot.year.sd$NMDS2)

O_NMDS.plot.year <- plyr::rename(O_NMDS.plot.year, c("plot.year" = "plot.year", "NMDS1" = "NMDS1.mean",
                                                     "O_NMDS1.plot.year.sd$NMDS1" = "NMDS1.sd",
                                                     "O_NMDS2.plot.year.mean$NMDS2" = "NMDS2.mean",  
                                                     "O_NMDS2.plot.year.sd$NMDS2" = "NMDS2.sd"))
O_NMDS.plot.year$plot <- c("N", "N", "O", "O")
O_NMDS.plot.year$year <- c(1, 3, 1, 3)
O_scores_managed$plot[O_scores_managed$plot == "O"] <- "O"

O_scores_managed <- O_scores_managed[-c(39), ]

ggplot() + geom_point(data = O_scores_managed, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.5) +
  geom_point(data = O_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4) + 
  geom_errorbar(data = O_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = O_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +           
  scale_color_manual(name = "Plot", values = c("N" = "royalblue4", "O" = "goldenrod")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17)) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-0.9, 1.3), breaks = seq(-0.9, 1.3, 0.44)) +
  scale_y_continuous(limits = c(-1, 0.7), breaks = seq(-1, 0.7, 0.34)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

### UNMANAGED

O_scores_reference <- O_scores[O_scores$area == "R", ]
O_NMDS1.plot.year.mean <- aggregate(NMDS1 ~ plot.year, O_scores_reference, mean) # x coordinate of centroid
O_NMDS1.plot.year.sd <- aggregate(NMDS1 ~ plot.year, O_scores_reference, sd) # x error bar
O_NMDS2.plot.year.mean <- aggregate(NMDS2 ~ plot.year, O_scores_reference, mean) # y coordinate of centroid
O_NMDS2.plot.year.sd <- aggregate(NMDS2 ~ plot.year, O_scores_reference, sd) # y error bar
O_NMDS.plot.year <- cbind.data.frame(O_NMDS1.plot.year.mean, O_NMDS1.plot.year.sd$NMDS1,
                                     O_NMDS2.plot.year.mean$NMDS2, O_NMDS2.plot.year.sd$NMDS2)

O_NMDS.plot.year <- plyr::rename(O_NMDS.plot.year, c("plot.year" = "plot.year", "NMDS1" = "NMDS1.mean",
                                                     "O_NMDS1.plot.year.sd$NMDS1" = "NMDS1.sd",
                                                     "O_NMDS2.plot.year.mean$NMDS2" = "NMDS2.mean",  
                                                     "O_NMDS2.plot.year.sd$NMDS2" = "NMDS2.sd"))
O_NMDS.plot.year$plot <- c("N", "N", "O", "O")
O_NMDS.plot.year$year <- c(1, 3, 1, 3)
O_scores_reference$plot[O_scores_reference$plot == "O"] <- "O"

O_scores_reference <- O_scores_reference[-c(2, 5), ]

ggplot() + geom_point(data = O_scores_reference, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.5) +
  geom_point(data = O_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4) + 
  geom_errorbar(data = O_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0)) +
  geom_errorbar(data = O_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0)) +           
  scale_color_manual(name = "Plot", values = c("N" = "royalblue4", "O" = "goldenrod")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17)) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-1.2, 0.7), breaks = seq(-1.2, 0.7, 0.38)) +
  scale_y_continuous(limits = c(-0.6, 0.5), breaks = seq(-0.6, 0.5, 0.22)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))




##### II. NON-INVASIVE COMMUNITY

### SITE-YEAR NMDS

veg_dat_noinv <- veg_dat[, -c(12, 19, 28, 46, 50, 69, 70)]

deleted_rows2 <- which(rowSums(veg_dat_noinv[, 7:82]) == 0) # Taking out rows with 0% total cover

veg_dat_noinv <- veg_dat_noinv[-deleted_rows2,] 

# m13 <- metaMDS(veg_dat_noinv[, 7:82], k = 2, trymax = 100)
# m13
# plot(m13, type = "t")

# 114

# m14 <- metaMDS(veg_dat_noinv[-c(101), 7:82], k = 2, trymax = 100)
# m14
# plot(m14, type = "t")

# quads 22 and 84 are now sorta outliers

m15 <- metaMDS(veg_dat_noinv[-c(17, 76, 101), 7:82], k = 2, trymax = 100)
m15
plot(m15, type = "t")

# all the necessary variables
com.scores <- as.data.frame(scores(m15))
com.scores$year <- veg_dat_noinv[-c(17, 76, 101), 5]
com.scores$site <- veg_dat_noinv[-c(17, 76, 101), 1]
com.scores$area <- veg_dat_noinv[-c(17, 76, 101), 2]
com.scores$plot <- veg_dat_noinv[-c(17, 76, 101), 3]
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

ggplot() + geom_point(data = com.scores, aes(x = NMDS1, y = NMDS2, color = site, shape = as.factor(year)), alpha = 0.5) +
  geom_point(data = NMDS.site.year, aes(x = NMDS1.mean, y = NMDS2.mean, color = site, shape = as.factor(year)), size = 4) + 
  geom_errorbar(data = NMDS.site.year, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0, color = site)) +
  geom_errorbar(data = NMDS.site.year, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0, color = site)) +           
  scale_color_manual(name = "Site", values = c("BM" = "royalblue4", "GL" = "goldenrod", "TG" = "green4")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17)) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-1, 1)) +
  scale_y_continuous(limits = c(-1, 1)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

### AREA-PLOT-YEAR NMDS

## INVADED DATA [FENCING COMPARISON]

invaded_dat <- veg_dat_noinv[veg_dat_noinv$plot != "N", ]

which(rowSums(invaded_dat[, 7:82]) == 0) # all rows still contain some veg

# m16 <- metaMDS(invaded_dat[, 7:82], k = 2, trymax = 100)
# m16
# plot(m16, type = "t")

#114

# m17 <- metaMDS(invaded_dat[-c(70), 7:82], k = 2, trymax = 100)
# m17
# plot(m17, type = "t")

#84

# m18 <- metaMDS(invaded_dat[-c(53, 70), 7:82], k = 2, trymax = 100)
# m18
# plot(m18, type = "t")

# 43

m19 <- metaMDS(invaded_dat[-c(30, 53, 70), 7:82], k = 2, trymax = 100)
m19
plot(m19, type = "t")

I_scores <- as.data.frame(scores(m19))
I_scores$year <- invaded_dat[-c(30, 53, 70), 5]
I_scores$plot <- invaded_dat[-c(30, 53, 70), 3]
I_scores$area <- invaded_dat[-c(30, 53, 70), 2]
I_scores$plot.year <- as.factor(paste(I_scores$plot, I_scores$year, sep = ""))

### MANAGED

I_scores_managed <- I_scores[I_scores$area == "M", ]
I_NMDS1.plot.year.mean <- aggregate(NMDS1 ~ plot.year, I_scores_managed, mean) # x coordinate of centroid
I_NMDS1.plot.year.sd <- aggregate(NMDS1 ~ plot.year, I_scores_managed, sd) # x error bar
I_NMDS2.plot.year.mean <- aggregate(NMDS2 ~ plot.year, I_scores_managed, mean) # y coordinate of centroid
I_NMDS2.plot.year.sd <- aggregate(NMDS2 ~ plot.year, I_scores_managed, sd) # y error bar
I_NMDS.plot.year <- cbind.data.frame(I_NMDS1.plot.year.mean, I_NMDS1.plot.year.sd$NMDS1,
                                     I_NMDS2.plot.year.mean$NMDS2, I_NMDS2.plot.year.sd$NMDS2)

I_NMDS.plot.year <- plyr::rename(I_NMDS.plot.year, c("plot.year" = "plot.year", "NMDS1" = "NMDS1.mean",
                                                     "I_NMDS1.plot.year.sd$NMDS1" = "NMDS1.sd",
                                                     "I_NMDS2.plot.year.mean$NMDS2" = "NMDS2.mean",  
                                                     "I_NMDS2.plot.year.sd$NMDS2" = "NMDS2.sd"))
I_NMDS.plot.year$plot <- c("F", "F", "O", "O")
I_NMDS.plot.year$year <- c(1, 3, 1, 3)

I_scores_managed <- I_scores_managed[-c(39), ]

ggplot() + geom_point(data = I_scores_managed, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.5) +
  geom_point(data = I_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4) + 
  geom_errorbar(data = I_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0, color = plot)) +
  geom_errorbar(data = I_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0, color = plot)) +           
  scale_color_manual(name = "Plot", values = c("F" = "green4", "O" = "goldenrod")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17)) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-0.8, 1.2), breaks = seq(-0.8, 1.2, 0.5)) +
  scale_y_continuous(limits = c(-1.2, 0.8), breaks = seq(-1.2, 0.8, 0.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

### UNMANAGED

I_scores_reference <- I_scores[I_scores$area == "R", ]
I_NMDS1.plot.year.mean <- aggregate(NMDS1 ~ plot.year, I_scores_reference, mean) # x coordinate of centroid
I_NMDS1.plot.year.sd <- aggregate(NMDS1 ~ plot.year, I_scores_reference, sd) # x error bar
I_NMDS2.plot.year.mean <- aggregate(NMDS2 ~ plot.year, I_scores_reference, mean) # y coordinate of centroid
I_NMDS2.plot.year.sd <- aggregate(NMDS2 ~ plot.year, I_scores_reference, sd) # y error bar
I_NMDS.plot.year <- cbind.data.frame(I_NMDS1.plot.year.mean, I_NMDS1.plot.year.sd$NMDS1,
                                     I_NMDS2.plot.year.mean$NMDS2, I_NMDS2.plot.year.sd$NMDS2)

I_NMDS.plot.year <- plyr::rename(I_NMDS.plot.year, c("plot.year" = "plot.year", "NMDS1" = "NMDS1.mean",
                                                     "I_NMDS1.plot.year.sd$NMDS1" = "NMDS1.sd",
                                                     "I_NMDS2.plot.year.mean$NMDS2" = "NMDS2.mean",  
                                                     "I_NMDS2.plot.year.sd$NMDS2" = "NMDS2.sd"))
I_NMDS.plot.year$plot <- c("F", "F", "O", "O")
I_NMDS.plot.year$year <- c(1, 3, 1, 3)


I_scores_reference <- I_scores_reference[-c(2, 5), ]

ggplot() + geom_point(data = I_scores_reference, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.5) +
  geom_point(data = I_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4) + 
  geom_errorbar(data = I_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0, color = plot)) +
  geom_errorbar(data = I_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0, color = plot)) +           
  scale_color_manual(name = "Plot", values = c("F" = "green4", "O" = "goldenrod")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17)) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-1.1, 0.9), breaks = seq(-1.1, 0.9, 0.5)) +
  scale_y_continuous(limits = c(-0.7, 0.9), breaks = seq(-0.7, 1.3, 0.4)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))


### OPEN DATA [INVASION COMPARISON]

open_dat <- veg_dat_noinv[veg_dat_noinv$plot != "F", ]

which(rowSums(open_dat[, 7:82]) == 0) # all rows still contain some veg

# m20 <- metaMDS(open_dat[, 7:82], k = 2, trymax = 100)
# m20
# plot(m20, type = "t")

# 22

# m21 <- metaMDS(open_dat[-c(8), 7:82], k = 2, trymax = 100)
# m21
# plot(m21, type = "t")

# 46 + 23

# m22 <- metaMDS(open_dat[-c(8, 9, 21), 7:82], k = 2, trymax = 100)
# m22
# plot(m22, type = "t")

# 282

m23 <- metaMDS(open_dat[-c(8, 9, 21, 166), 7:82], k = 2, trymax = 100)
m23
plot(m23, type = "t")

O_scores <- as.data.frame(scores(m23))
O_scores$year <- open_dat[-c(8, 9, 21, 166), 5]
O_scores$area <- open_dat[-c(8, 9, 21, 166), 2]
O_scores$plot <- open_dat[-c(8, 9, 21, 166), 3]
O_scores$plot.year <- as.factor(paste(O_scores$plot, O_scores$year, sep = ""))

### MANAGED

O_scores_managed <- O_scores[O_scores$area == "M", ]
O_NMDS1.plot.year.mean <- aggregate(NMDS1 ~ plot.year, O_scores_managed, mean) # x coordinate of centroid
O_NMDS1.plot.year.sd <- aggregate(NMDS1 ~ plot.year, O_scores_managed, sd) # x error bar
O_NMDS2.plot.year.mean <- aggregate(NMDS2 ~ plot.year, O_scores_managed, mean) # y coordinate of centroid
O_NMDS2.plot.year.sd <- aggregate(NMDS2 ~ plot.year, O_scores_managed, sd) # y error bar
O_NMDS.plot.year <- cbind.data.frame(O_NMDS1.plot.year.mean, O_NMDS1.plot.year.sd$NMDS1,
                                     O_NMDS2.plot.year.mean$NMDS2, O_NMDS2.plot.year.sd$NMDS2)

O_NMDS.plot.year <- plyr::rename(O_NMDS.plot.year, c("plot.year" = "plot.year", "NMDS1" = "NMDS1.mean",
                                                     "O_NMDS1.plot.year.sd$NMDS1" = "NMDS1.sd",
                                                     "O_NMDS2.plot.year.mean$NMDS2" = "NMDS2.mean",  
                                                     "O_NMDS2.plot.year.sd$NMDS2" = "NMDS2.sd"))
O_NMDS.plot.year$plot <- c("N", "N", "O", "O")
O_NMDS.plot.year$year <- c(1, 3, 1, 3)
O_scores_managed$plot[O_scores_managed$plot == "O"] <- "O"

O_scores_managed <- O_scores_managed[-c(39), ]

ggplot() + geom_point(data = O_scores_managed, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.5) +
  geom_point(data = O_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4) + 
  geom_errorbar(data = O_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0, color = plot)) +
  geom_errorbar(data = O_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0, color = plot)) +           
  scale_color_manual(name = "Plot", values = c("N" = "royalblue4", "O" = "goldenrod")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17)) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-1.5, 1.7), breaks = seq(-1.5, 1.7, 0.8)) +
  scale_y_continuous(limits = c(-1, 1.2), breaks = seq(-1, 1.2, 0.55)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

### UNMANAGED

O_scores_reference <- O_scores[O_scores$area == "R", ]
O_NMDS1.plot.year.mean <- aggregate(NMDS1 ~ plot.year, O_scores_reference, mean) # x coordinate of centroid
O_NMDS1.plot.year.sd <- aggregate(NMDS1 ~ plot.year, O_scores_reference, sd) # x error bar
O_NMDS2.plot.year.mean <- aggregate(NMDS2 ~ plot.year, O_scores_reference, mean) # y coordinate of centroid
O_NMDS2.plot.year.sd <- aggregate(NMDS2 ~ plot.year, O_scores_reference, sd) # y error bar
O_NMDS.plot.year <- cbind.data.frame(O_NMDS1.plot.year.mean, O_NMDS1.plot.year.sd$NMDS1,
                                     O_NMDS2.plot.year.mean$NMDS2, O_NMDS2.plot.year.sd$NMDS2)

O_NMDS.plot.year <- plyr::rename(O_NMDS.plot.year, c("plot.year" = "plot.year", "NMDS1" = "NMDS1.mean",
                                                     "O_NMDS1.plot.year.sd$NMDS1" = "NMDS1.sd",
                                                     "O_NMDS2.plot.year.mean$NMDS2" = "NMDS2.mean",  
                                                     "O_NMDS2.plot.year.sd$NMDS2" = "NMDS2.sd"))
O_NMDS.plot.year$plot <- c("N", "N", "O", "O")
O_NMDS.plot.year$year <- c(1, 3, 1, 3)
O_scores_reference$plot[O_scores_reference$plot == "O"] <- "O"

O_scores_reference <- O_scores_reference[-c(2, 5), ]

ggplot() + geom_point(data = O_scores_reference, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.5) +
  geom_point(data = O_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4) + 
  geom_errorbar(data = O_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0, color = plot)) +
  geom_errorbar(data = O_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0, color = plot)) +           
  scale_color_manual(name = "Plot", values = c("N" = "royalblue4", "O" = "goldenrod")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17)) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-1.2, 0.8), breaks = seq(-1.2, 0.8, 0.5)) +
  scale_y_continuous(limits = c(-0.8, 0.6), breaks = seq(-0.8, 0.6, 0.35)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))






##### III. NATIVE COMMUNITY


### SITE-YEAR NMDS

veg_dat_nat <- veg_dat_noinv[, -c(12, 21, 29, 31, 36, 39, 42, 43, 49, 57, 61, 64, 70, 71, 76:84)]

deleted_rows3 <- which(rowSums(veg_dat_nat[, 7:61]) == 0) # Taking out rows with 0% total cover

veg_dat_nat <- veg_dat_nat[-deleted_rows3,] 

# m24 <- metaMDS(veg_dat_nat[, 7:61], k = 2, trymax = 100)
# m24
# plot(m24, type = "t")

# 134

# m25 <- metaMDS(veg_dat_nat[-c(108), 7:61], k = 2, trymax = 100)
# m25
# plot(m25, type = "t")

# 22 and 84

m26 <- metaMDS(veg_dat_nat[-c(16, 75, 108), 7:61], k = 2, trymax = 100)
m26
plot(m26, type = "t")

# all the necessary variables
com.scores <- as.data.frame(scores(m25))
com.scores$year <- veg_dat_nat[-c(108), 5]
com.scores$site <- veg_dat_nat[-c(108), 1]
com.scores$area <- veg_dat_nat[-c(108), 2]
com.scores$plot <- veg_dat_nat[-c(108), 3]
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

ggplot() + geom_point(data = com.scores, aes(x = NMDS1, y = NMDS2, color = site, shape = as.factor(year)), alpha = 0.5) +
  geom_point(data = NMDS.site.year, aes(x = NMDS1.mean, y = NMDS2.mean, color = site, shape = as.factor(year)), size = 4) + 
  geom_errorbar(data = NMDS.site.year, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0, color = site)) +
  geom_errorbar(data = NMDS.site.year, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0, color = site)) +           
  scale_color_manual(name = "Site", values = c("BM" = "royalblue4", "GL" = "goldenrod", "TG" = "green4")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17)) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-2, 2)) +
  scale_y_continuous(limits = c(-1, 1)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))


### AREA-PLOT-YEAR NMDS

## INVADED DATA [FENCING COMPARISON]

invaded_dat <- veg_dat_nat[veg_dat_nat$plot != "N", ]

which(rowSums(invaded_dat[, 7:61]) == 0) # all rows still contain some veg

# m27 <- metaMDS(invaded_dat[, 7:61], k = 2, trymax = 100)
# m27
# plot(m27, type = "t")

# 43 + 45

# m28 <- metaMDS(invaded_dat[-c(29, 31), 7:61], k = 2, trymax = 100)
# m28
# plot(m28, type = "t")

# 84

m29 <- metaMDS(invaded_dat[-c(29, 31, 52), 7:61], k = 2, trymax = 100)
m29
plot(m29, type = "t")


I_scores <- as.data.frame(scores(m29))
I_scores$year <- invaded_dat[-c(29, 31, 52), 5]
I_scores$plot <- invaded_dat[-c(29, 31, 52), 3]
I_scores$area <- invaded_dat[-c(29, 31, 52), 2]
I_scores$plot.year <- as.factor(paste(I_scores$plot, I_scores$year, sep = ""))

### MANAGED

I_scores_managed <- I_scores[I_scores$area == "M", ]
I_NMDS1.plot.year.mean <- aggregate(NMDS1 ~ plot.year, I_scores_managed, mean) # x coordinate of centroid
I_NMDS1.plot.year.sd <- aggregate(NMDS1 ~ plot.year, I_scores_managed, sd) # x error bar
I_NMDS2.plot.year.mean <- aggregate(NMDS2 ~ plot.year, I_scores_managed, mean) # y coordinate of centroid
I_NMDS2.plot.year.sd <- aggregate(NMDS2 ~ plot.year, I_scores_managed, sd) # y error bar
I_NMDS.plot.year <- cbind.data.frame(I_NMDS1.plot.year.mean, I_NMDS1.plot.year.sd$NMDS1,
                                     I_NMDS2.plot.year.mean$NMDS2, I_NMDS2.plot.year.sd$NMDS2)

I_NMDS.plot.year <- plyr::rename(I_NMDS.plot.year, c("plot.year" = "plot.year", "NMDS1" = "NMDS1.mean",
                                                     "I_NMDS1.plot.year.sd$NMDS1" = "NMDS1.sd",
                                                     "I_NMDS2.plot.year.mean$NMDS2" = "NMDS2.mean",  
                                                     "I_NMDS2.plot.year.sd$NMDS2" = "NMDS2.sd"))
I_NMDS.plot.year$plot <- c("F", "F", "O", "O")
I_NMDS.plot.year$year <- c(1, 3, 1, 3)

I_scores_managed <- I_scores_managed[-c(39), ]

ggplot() + geom_point(data = I_scores_managed, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.5) +
  geom_point(data = I_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4) + 
  geom_errorbar(data = I_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0, color = plot)) +
  geom_errorbar(data = I_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0, color = plot)) +           
  scale_color_manual(name = "Plot", values = c("F" = "green4", "O" = "goldenrod")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17)) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-2.5, 1.7), breaks = seq(-2.5, 1.7, 1.05)) +
  scale_y_continuous(limits = c(-1.3, 1.1), breaks = seq(-1.3, 1.1, 0.6)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

### UNMANAGED

I_scores_reference <- I_scores[I_scores$area == "R", ]
I_NMDS1.plot.year.mean <- aggregate(NMDS1 ~ plot.year, I_scores_reference, mean) # x coordinate of centroid
I_NMDS1.plot.year.sd <- aggregate(NMDS1 ~ plot.year, I_scores_reference, sd) # x error bar
I_NMDS2.plot.year.mean <- aggregate(NMDS2 ~ plot.year, I_scores_reference, mean) # y coordinate of centroid
I_NMDS2.plot.year.sd <- aggregate(NMDS2 ~ plot.year, I_scores_reference, sd) # y error bar
I_NMDS.plot.year <- cbind.data.frame(I_NMDS1.plot.year.mean, I_NMDS1.plot.year.sd$NMDS1,
                                     I_NMDS2.plot.year.mean$NMDS2, I_NMDS2.plot.year.sd$NMDS2)

I_NMDS.plot.year <- plyr::rename(I_NMDS.plot.year, c("plot.year" = "plot.year", "NMDS1" = "NMDS1.mean",
                                                     "I_NMDS1.plot.year.sd$NMDS1" = "NMDS1.sd",
                                                     "I_NMDS2.plot.year.mean$NMDS2" = "NMDS2.mean",  
                                                     "I_NMDS2.plot.year.sd$NMDS2" = "NMDS2.sd"))
I_NMDS.plot.year$plot <- c("F", "F", "O", "O")
I_NMDS.plot.year$year <- c(1, 3, 1, 3)


I_scores_reference <- I_scores_reference[-c(2, 5), ]

ggplot() + geom_point(data = I_scores_reference, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.5) +
  geom_point(data = I_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4) + 
  geom_errorbar(data = I_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0, color = plot)) +
  geom_errorbar(data = I_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0, color = plot)) +           
  scale_color_manual(name = "Plot", values = c("F" = "green4", "O" = "goldenrod")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17)) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-1.2, 0.8), breaks = seq(-1.2, 0.8, 0.5)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))


### OPEN DATA [INVASION COMPARISON]

open_dat <- veg_dat_nat[veg_dat_nat$plot != "F", ]

which(rowSums(open_dat[, 7:61]) == 0) # all rows still contain some veg

# m30 <- metaMDS(open_dat[, 7:61], k = 2, trymax = 100)
# m30
# plot(m30, type = "t")

# 22

# m31 <- metaMDS(open_dat[-c(7), 7:61], k = 2, trymax = 100)
# m31
# plot(m31, type = "t")

# 134

# m32 <- metaMDS(open_dat[-c(7, 70), 7:61], k = 2, trymax = 100)
# m32
# plot(m32, type = "t")

# 16, 23, 46

m33 <- metaMDS(open_dat[-c(5, 7, 8, 20, 70), 7:61], k = 2, trymax = 100)
m33
plot(m33, type = "t")

O_scores <- as.data.frame(scores(m33))
O_scores$year <- open_dat[-c(5, 7, 8, 20, 70), 5]
O_scores$area <- open_dat[-c(5, 7, 8, 20, 70), 2]
O_scores$plot <- open_dat[-c(5, 7, 8, 20, 70), 3]
O_scores$plot.year <- as.factor(paste(O_scores$plot, O_scores$year, sep = ""))

### MANAGED

O_scores_managed <- O_scores[O_scores$area == "M", ]
O_NMDS1.plot.year.mean <- aggregate(NMDS1 ~ plot.year, O_scores_managed, mean) # x coordinate of centroid
O_NMDS1.plot.year.sd <- aggregate(NMDS1 ~ plot.year, O_scores_managed, sd) # x error bar
O_NMDS2.plot.year.mean <- aggregate(NMDS2 ~ plot.year, O_scores_managed, mean) # y coordinate of centroid
O_NMDS2.plot.year.sd <- aggregate(NMDS2 ~ plot.year, O_scores_managed, sd) # y error bar
O_NMDS.plot.year <- cbind.data.frame(O_NMDS1.plot.year.mean, O_NMDS1.plot.year.sd$NMDS1,
                                     O_NMDS2.plot.year.mean$NMDS2, O_NMDS2.plot.year.sd$NMDS2)

O_NMDS.plot.year <- plyr::rename(O_NMDS.plot.year, c("plot.year" = "plot.year", "NMDS1" = "NMDS1.mean",
                                                     "O_NMDS1.plot.year.sd$NMDS1" = "NMDS1.sd",
                                                     "O_NMDS2.plot.year.mean$NMDS2" = "NMDS2.mean",  
                                                     "O_NMDS2.plot.year.sd$NMDS2" = "NMDS2.sd"))
O_NMDS.plot.year$plot <- c("N", "N", "O", "O")
O_NMDS.plot.year$year <- c(1, 3, 1, 3)
O_scores_managed$plot[O_scores_managed$plot == "O"] <- "O"

O_scores_managed <- O_scores_managed[-c(39), ]

ggplot() + geom_point(data = O_scores_managed, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.5) +
  geom_point(data = O_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4) + 
  geom_errorbar(data = O_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0, color = plot)) +
  geom_errorbar(data = O_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0, color = plot)) +           
  scale_color_manual(name = "Plot", values = c("N" = "royalblue4", "O" = "goldenrod")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17)) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-1, 2.2), breaks = seq(-1, 2.2, 0.8)) +
  scale_y_continuous(limits = c(-1, 1), breaks = seq(-1, 1, 0.5)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))

### UNMANAGED

O_scores_reference <- O_scores[O_scores$area == "R", ]
O_NMDS1.plot.year.mean <- aggregate(NMDS1 ~ plot.year, O_scores_reference, mean) # x coordinate of centroid
O_NMDS1.plot.year.sd <- aggregate(NMDS1 ~ plot.year, O_scores_reference, sd) # x error bar
O_NMDS2.plot.year.mean <- aggregate(NMDS2 ~ plot.year, O_scores_reference, mean) # y coordinate of centroid
O_NMDS2.plot.year.sd <- aggregate(NMDS2 ~ plot.year, O_scores_reference, sd) # y error bar
O_NMDS.plot.year <- cbind.data.frame(O_NMDS1.plot.year.mean, O_NMDS1.plot.year.sd$NMDS1,
                                     O_NMDS2.plot.year.mean$NMDS2, O_NMDS2.plot.year.sd$NMDS2)

O_NMDS.plot.year <- plyr::rename(O_NMDS.plot.year, c("plot.year" = "plot.year", "NMDS1" = "NMDS1.mean",
                                                     "O_NMDS1.plot.year.sd$NMDS1" = "NMDS1.sd",
                                                     "O_NMDS2.plot.year.mean$NMDS2" = "NMDS2.mean",  
                                                     "O_NMDS2.plot.year.sd$NMDS2" = "NMDS2.sd"))
O_NMDS.plot.year$plot <- c("N", "N", "O", "O")
O_NMDS.plot.year$year <- c(1, 3, 1, 3)
O_scores_reference$plot[O_scores_reference$plot == "O"] <- "O"

O_scores_reference <- O_scores_reference[-c(2, 5), ]

ggplot() + geom_point(data = O_scores_reference, aes(x = NMDS1, y = NMDS2, color = plot, shape = as.factor(year)), alpha = 0.5) +
  geom_point(data = O_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, color = plot, shape = as.factor(year)), size = 4) + 
  geom_errorbar(data = O_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, xmin = NMDS1.mean - NMDS1.sd, xmax = NMDS1.mean + NMDS1.sd, width = 0, color = plot)) +
  geom_errorbar(data = O_NMDS.plot.year, aes(x = NMDS1.mean, y = NMDS2.mean, ymin = NMDS2.mean - NMDS2.sd, ymax = NMDS2.mean + NMDS2.sd, width = 0, color = plot)) +           
  scale_color_manual(name = "Plot", values = c("N" = "royalblue4", "O" = "goldenrod")) + 
  scale_shape_manual(name = "Year", values = c("1" = 15, "3" = 17)) +
  theme_classic(base_size = 12) +
  scale_x_continuous(limits = c(-1, 0.8), breaks = seq(-1, 0.8, 0.45)) +
  scale_y_continuous(limits = c(-0.8, 1), breaks = seq(-0.8, 1, 0.45)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
