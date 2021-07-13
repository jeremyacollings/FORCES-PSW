########## MVABUND FOR FORCES PSW PROJECT ##########

# Short description

library(tidyverse)
library(mvabund)
library(patchwork)

##### I. Whole PLANT COMMUNITY

### PREPARING DATA FOR MVABUND

year <- c(rep(1, times = 162), rep(3, times = 162))
worm.mass <- mydata$worms$ew.mass.mean
worm.count <- mydata$worms$ew.count.mean
psw <- mydata$veg$vinros
plot_type <- as.factor(paste(mydata$identifiers$site, 
                             mydata$identifiers$area, 
                             mydata$identifiers$plot, 
                             sep = ""))
fencing <- ifelse(mydata$identifiers$plot == "F", "F", "O")
invaded <- ifelse(mydata$identifiers$plot == "N", "uninv", "inv")
com.dat <- cbind.data.frame("site" = mydata$identifiers$site, "area" = mydata$identifiers$area, 
                            "plot" = mydata$identifiers$plot, "fencing" = fencing, 
                            "invaded" = invaded, "plot_type" = plot_type,
                            "year" = year, "worm.mass" = worm.mass, "worm.count" = worm.count,
                            "psw" = psw, round(mydata$veg_noPSW))

com.dat.fence <- com.dat[com.dat$invaded == "inv", ]
com.dat.invade <- com.dat[com.dat$fencing == "O", ]

com_bund_fence <- mvabund(com.dat.fence[,11:93]) # transforms community data into mvabund appropriate object
com_bund_invade <- mvabund(com.dat.invade[,11:93])
### FITTING MODELS

mva1 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$plot +   # complete model
                com.dat$worm.count + com.dat$year:com.dat$plot +
                com.dat$area:com.dat$plot + com.dat$year:com.dat$area + 
                com.dat$year:com.dat$plot:com.dat$area)


mva2 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$plot +    # compare count vs mass
                  com.dat$worm.mass + com.dat$year:com.dat$plot +
                  com.dat$area:com.dat$plot + com.dat$year:com.dat$area + 
                  com.dat$year:com.dat$plot:com.dat$area)

mva3 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$plot +   # compare three-way interaction
                  com.dat$worm.count + com.dat$year:com.dat$plot +
                  com.dat$area:com.dat$plot + com.dat$year:com.dat$area)

mva4 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$plot +   # compare year-area
                 com.dat$worm.count + com.dat$year:com.dat$plot +
                 com.dat$area:com.dat$plot + com.dat$year:com.dat$plot:com.dat$area)

mva5 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$plot +   # compare area-plot
          com.dat$worm.count + com.dat$year:com.dat$plot +
          com.dat$year:com.dat$area + com.dat$year:com.dat$plot:com.dat$area)

mva6 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$plot +   # compare year-plot
          com.dat$worm.count + com.dat$area:com.dat$plot + 
          com.dat$year:com.dat$area + com.dat$year:com.dat$plot:com.dat$area)

mva7 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$plot +   # compare worms
          com.dat$year:com.dat$plot + com.dat$area:com.dat$plot + 
          com.dat$year:com.dat$area + com.dat$year:com.dat$plot:com.dat$area)

mva8 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$worm.count) # compare plot

mva9 <- manyglm(com_bund ~ com.dat$year + com.dat$plot + com.dat$worm.count) # compare area

mva10 <- manyglm(com_bund ~ com.dat$area + com.dat$plot + com.dat$worm.count) # compare year

### MODEL COMPARISON

# to account for nested data

control <- how(within = Within(type = 'none'),
               plots = Plots(strata = com.dat$plot_type, type = 'free'),
               nperm = 1000)

permutations <- shuffleSet(nrow(com_bund), control = control)

lr_worm.measure <- anova(mva1, mva2, bootID = permutations, p.uni = 'adjusted', test = 'LR')

lr_YPA <- anova(mva1, mva3, bootID = permutations, p.uni = 'adjusted', test = 'LR')
lr_YPA

lr_YA <- anova(mva1, mva4, bootID = permutations, p.uni = 'adjusted', test = 'LR')
lr_YA

lr_AP <- anova(mva1, mva5, bootID = permutations, p.uni = 'adjusted', test = 'LR')
lr_AP

lr_YP <- anova(mva1, mva6, bootID = permutations, p.uni = 'adjusted', test = 'LR')
lr_YP

lr_worm <- anova(mva1, mva7, bootID = permutations, p.uni = 'adjusted', test = 'LR')
lr_worm

lr_plot <- anova(mva1, mva8, bootID = permutations, p.uni = 'adjusted', test = 'LR')
lr_plot

lr_area <- anova(mva1, mva9, bootID = permutations, p.uni = 'adjusted', test = 'LR')
lr_area

lr_year <- anova(mva1, mva10, bootID = permutations, p.uni = 'adjusted', test = 'LR')
lr_year

# I dont know why the worm measurement comparisons arent working... saving without for now

saveRDS(lr_YPA, "YPA Whole Community.rds")
saveRDS(lr_YA, "YA Whole Community.rds")
saveRDS(lr_AP, "AP Whole Community.rds")
saveRDS(lr_YP, "YP Whole Community.rds")
saveRDS(lr_worm, "Worm Whole Community.rds")
saveRDS(lr_plot, "Plot Whole Community.rds")
saveRDS(lr_area, "Area Whole Community.rds")
saveRDS(lr_year, "Year Whole Community.rds")









##### II. ONLY NON-INVASIVE PLANTS

### PREPARING DATA FOR MVABUND

all <- mydata$veg_noPSW
notinvasives <- all[-c(6, 13, 22, 40, 44, 63, 64)]
natives <- notinvasives[-c(6, 15, 23, 25, 30, 33, 36, 37, 43, 51, 55, 58, 64, 65, 70:78)]

com.dat2 <- cbind.data.frame("site" = mydata$identifiers$site, "area" = mydata$identifiers$area, 
                         "plot" = mydata$identifiers$plot, "plot_type" = plot_type, 
                         "fencing" = fencing, "invaded" = invaded,
                         "year" = year, "worm.mass" = worm.mass, "worm.count" = worm.count,
                         "psw" = psw, round(notinvasives))

com_bund2 <- mvabund(com.dat2[,11:86])

### FITTING MODELS

mva11 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$plot +   # complete model
                   com.dat2$worm.count + com.dat2$year:com.dat2$plot +
                   com.dat2$area:com.dat2$plot + com.dat2$year:com.dat2$area + 
                   com.dat2$year:com.dat2$plot:com.dat2$area)

mva12 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$plot +    # compare count vs mass
                   com.dat2$worm.mass + com.dat2$year:com.dat2$plot +
                   com.dat2$area:com.dat2$plot + com.dat2$year:com.dat2$area + 
                   com.dat2$year:com.dat2$plot:com.dat2$area)

mva13 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$plot +   # compare three-way interaction
                   com.dat2$worm.count + com.dat2$year:com.dat2$plot +
                   com.dat2$area:com.dat2$plot + com.dat2$year:com.dat2$area)

mva14 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$plot +   # compare year-area
                   com.dat2$worm.count + com.dat2$year:com.dat2$plot +
                   com.dat2$area:com.dat2$plot + com.dat2$year:com.dat2$plot:com.dat2$area)

mva15 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$plot +   # compare area-plot
                   com.dat2$worm.count + com.dat2$year:com.dat2$plot +
                   com.dat2$year:com.dat2$area + com.dat2$year:com.dat2$plot:com.dat2$area)

mva16 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$plot +   # compare year-plot
                   com.dat2$worm.count + com.dat2$area:com.dat2$plot + 
                   com.dat2$year:com.dat2$area + com.dat2$year:com.dat2$plot:com.dat2$area)

mva17 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$plot +   # compare worms
                   com.dat2$year:com.dat2$plot + com.dat2$area:com.dat2$plot + 
                   com.dat2$year:com.dat2$area + com.dat2$year:com.dat2$plot:com.dat2$area)

mva18 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$worm.count) # compare plot

mva19 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$plot + com.dat2$worm.count) # compare area

mva20 <- manyglm(com_bund2 ~ com.dat2$area + com.dat2$plot + com.dat2$worm.count) # compare year

### MODEL COMPARISON

# to account for nested data

control2 <- how(within = Within(type = 'none'),
               plots = Plots(strata = com.dat2$plot_type, type = 'free'),
               nperm = 1000)

permutations2 <- shuffleSet(nrow(com_bund2), control = control2)

lr_worm.measure2 <- anova(mva11, mva12, bootID = permutations2, p.uni = 'adjusted', test = 'LR')

lr_YPA2 <- anova(mva11, mva13, bootID = permutations2, p.uni = 'adjusted', test = 'LR')

lr_YA2 <- anova(mva11, mva14, bootID = permutations2, p.uni = 'adjusted', test = 'LR')

lr_AP2 <- anova(mva11, mva15, bootID = permutations2, p.uni = 'adjusted', test = 'LR')

lr_YP2 <- anova(mva11, mva16, bootID = permutations2, p.uni = 'adjusted', test = 'LR')

lr_worm2 <- anova(mva11, mva17, bootID = permutations2, p.uni = 'adjusted', test = 'LR')

lr_plot2 <- anova(mva11, mva18, bootID = permutations2, p.uni = 'adjusted', test = 'LR')

lr_area2 <- anova(mva11, mva19, bootID = permutations2, p.uni = 'adjusted', test = 'LR')

lr_year2 <- anova(mva11, mva20, bootID = permutations2, p.uni = 'adjusted', test = 'LR')

saveRDS(lr_YPA2, "YPA Non-Invasive Community.rds")
saveRDS(lr_YA2, "YA Non-Invasive Community.rds")
saveRDS(lr_AP2, "AP Non-Invasive Community.rds")
saveRDS(lr_YP2, "YP Non-Invasive Community.rds")
saveRDS(lr_worm2, "Worm Non-Invasive Community.rds")
saveRDS(lr_plot2, "Plot Non-Invasive Community.rds")
saveRDS(lr_area2, "Area Non-Invasive Community.rds")
saveRDS(lr_year2, "Year Non-Invasive Community.rds")









##### III. ONLY NATIVE PLANTS

### PREPARING DATA FOR MVABUND

com.dat3 <- cbind.data.frame("site" = mydata$identifiers$site, "area" = mydata$identifiers$area, 
                         "plot" = mydata$identifiers$plot, "plot_type" = plot_type, 
                         "fencing" = fencing, "invaded" = invaded,
                         "year" = year, "worm.mass" = worm.mass, "worm.count" = worm.count,
                         "psw" = psw, round(natives))

com_bund3 <- mvabund(com.dat3[,11:65])

### FITTING MODELS


mva21 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$plot +   # complete model
                   com.dat3$worm.count + com.dat3$year:com.dat3$plot +
                   com.dat3$area:com.dat3$plot + com.dat3$year:com.dat3$area + 
                   com.dat3$year:com.dat3$plot:com.dat3$area)

mva22 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$plot +    # compare count vs mass
                   com.dat3$worm.mass + com.dat3$year:com.dat3$plot +
                   com.dat3$area:com.dat3$plot + com.dat3$year:com.dat3$area + 
                   com.dat3$year:com.dat3$plot:com.dat3$area)

mva23 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$plot +   # compare three-way interaction
                   com.dat3$worm.count + com.dat3$year:com.dat3$plot +
                   com.dat3$area:com.dat3$plot + com.dat3$year:com.dat3$area)

mva24 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$plot +   # compare year-area
                   com.dat3$worm.count + com.dat3$year:com.dat3$plot +
                   com.dat3$area:com.dat3$plot + com.dat3$year:com.dat3$plot:com.dat3$area)

mva25 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$plot +   # compare area-plot
                   com.dat3$worm.count + com.dat3$year:com.dat3$plot +
                   com.dat3$year:com.dat3$area + com.dat3$year:com.dat3$plot:com.dat3$area)

mva26 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$plot +   # compare year-plot
                   com.dat3$worm.count + com.dat3$area:com.dat3$plot + 
                   com.dat3$year:com.dat3$area + com.dat3$year:com.dat3$plot:com.dat3$area)

mva27 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$plot +   # compare worms
                   com.dat3$year:com.dat3$plot + com.dat3$area:com.dat3$plot + 
                   com.dat3$year:com.dat3$area + com.dat3$year:com.dat3$plot:com.dat3$area)

mva28 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$worm.count) # compare plot

mva29 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$plot + com.dat3$worm.count) # compare area

mva30 <- manyglm(com_bund3 ~ com.dat3$area + com.dat3$plot + com.dat3$worm.count) # compare year

### MODEL COMPARISON

# to account for nested data

control3 <- how(within = Within(type = 'none'),
                plots = Plots(strata = com.dat3$plot_type, type = 'free'),
                nperm = 1000)

permutations3 <- shuffleSet(nrow(com_bund3), control = control3)

lr_worm.measure3 <- anova(mva21, mva22, bootID = permutations3, p.uni = 'adjusted', test = 'LR')

lr_YPA3 <- anova(mva21, mva23, bootID = permutations3, p.uni = 'adjusted', test = 'LR')

lr_YA3 <- anova(mva21, mva24, bootID = permutations3, p.uni = 'adjusted', test = 'LR')

lr_AP3 <- anova(mva21, mva25, bootID = permutations3, p.uni = 'adjusted', test = 'LR')

lr_YP3 <- anova(mva21, mva26, bootID = permutations3, p.uni = 'adjusted', test = 'LR')

lr_worm3 <- anova(mva21, mva27, bootID = permutations3, p.uni = 'adjusted', test = 'LR')

lr_plot3 <- anova(mva21, mva28, bootID = permutations3, p.uni = 'adjusted', test = 'LR')

lr_area3 <- anova(mva21, mva29, bootID = permutations3, p.uni = 'adjusted', test = 'LR')

lr_year3 <- anova(mva21, mva30, bootID = permutations3, p.uni = 'adjusted', test = 'LR')

saveRDS(lr_YPA3, "YPA Native Community.rds")
saveRDS(lr_YA3, "YA Native Community.rds")
saveRDS(lr_AP3, "AP Native Community.rds")
saveRDS(lr_YP3, "YP Native Community.rds")
saveRDS(lr_worm3, "Worm Native Community.rds")
saveRDS(lr_plot3, "Plot Native Community.rds")
saveRDS(lr_area3, "Area Native Community.rds")
saveRDS(lr_year3, "Year Native Community.rds")









##### IV. MAKING THE PIE CHART FIGURE