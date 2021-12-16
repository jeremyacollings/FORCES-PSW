########## MVABUND FOR FORCES PSW PROJECT ##########

# Short description

library(tidyverse)
library(mvabund)
library(patchwork)

##### I. Whole PLANT COMMUNITY


# notes:
# make sure that swallow-wort is never in these analyses
# probably going to use the all plants in main manuscript
# non-natives and natives in supplemental
# "managed or not managed" or maybe unmanaged
# check the response from reviewers to make sure model structure looks good
# check other mvabund papers and report similarly
# find pie chart paper to check where deviance come from 


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
com.dat <- com.dat[, !(names(com.dat) %in% c("woody1", "woody2"))]
com.dat.fence <- com.dat[com.dat$invaded == "inv", ]
com.dat.invade <- com.dat[com.dat$fencing == "O", ]

com_bund_fence <- mvabund(com.dat.fence[,11:93]) # transforms community data into mvabund appropriate object
com_bund_invade <- mvabund(com.dat.invade[,11:93])

### FITTING MODELS FOR FENCING

control.F <- how(within = Within(type = 'none'),
                 plots = Plots(strata = com.dat.fence$plot_type, type = 'free'),
                 nperm = 1000)

permutations.F <- shuffleSet(nrow(com_bund_fence), control = control.F)

# starting with the full model:

fence.m0 <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area + com.dat.fence$plot +   # complete model
                     com.dat.fence$worm.count + 
                     com.dat.fence$area:com.dat.fence$plot + 
                     com.dat.fence$year:com.dat.fence$area + com.dat.fence$year:com.dat.fence$plot + 
                     com.dat.fence$year:com.dat.fence$area:com.dat.fence$plot)

# checking three-way interaction (Area:Plot:Year)

fence.m1 <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area + com.dat.fence$plot +   # no 3way
                     com.dat.fence$worm.count + 
                     com.dat.fence$area:com.dat.fence$plot + 
                     com.dat.fence$year:com.dat.fence$area + com.dat.fence$year:com.dat.fence$plot)

apy.f <- anova.manyglm(fence.m0, fence.m1, bootID = permutations.F, p.uni = 'adjusted', test = 'LR')

apy.f # dev = 20.79; p = 0.183

# three-way interaction came out as insignificant so I will now test the two-way interactions

# Area:Plot

fence.m2 <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area + 
                      com.dat.fence$plot + com.dat.fence$worm.count + 
                      com.dat.fence$year:com.dat.fence$area + 
                      com.dat.fence$year:com.dat.fence$plot)

ap.f <- anova.manyglm(fence.m1, fence.m2, bootID = permutations.F, p.uni = 'adjusted', test = 'LR')

ap.f # dev = 86.2; p = 0.234

# Area:Year

fence.m3 <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area + 
                      com.dat.fence$plot + com.dat.fence$worm.count + 
                      com.dat.fence$area:com.dat.fence$plot + 
                      com.dat.fence$year:com.dat.fence$plot)

ay.f <- anova.manyglm(fence.m1, fence.m3, bootID = permutations.F, p.uni = 'adjusted', test = 'LR')

ay.f # dev = 104.1; p = 0.002 **

# Plot:Year

fence.m4 <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area + 
                      com.dat.fence$plot + com.dat.fence$worm.count + 
                      com.dat.fence$year:com.dat.fence$area + 
                      com.dat.fence$area:com.dat.fence$plot)

py.f <- anova.manyglm(fence.m1, fence.m4, bootID = permutations.F, p.uni = 'adjusted', test = 'LR')

py.f # dev = 46.28; p = 0.509

# Area:Year interaction came out as significant but Area:Plot and 
# Plot:Year did not

# now checking non-interactive terms

fence.m5 <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area + 
                      com.dat.fence$plot + com.dat.fence$worm.count + 
                      com.dat.fence$year:com.dat.fence$area)
# Plot

fence.m6 <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area + 
                      com.dat.fence$worm.count + com.dat.fence$year:com.dat.fence$area)

p.f <- anova.manyglm(fence.m5, fence.m6, bootID = permutations.F, p.uni = 'adjusted', test = 'LR')

p.f # dev = 319.5; p = 0.001 ***
# Earthworms

fence.m7 <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area + 
                      com.dat.fence$plot + com.dat.fence$year:com.dat.fence$area)

ew.f <- anova.manyglm(fence.m5, fence.m7, bootID = permutations.F, p.uni = 'adjusted', test = 'LR')

ew.f # dev = 385.4; p = 0.001 ***

# both plot and earthworm count came out as significant

fence.final <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area + 
                        com.dat.fence$plot + com.dat.fence$worm.count + 
                        com.dat.fence$year:com.dat.fence$area)

fence.final.output <- anova.manyglm(fence.final, bootID = permutations.F, p.uni = "adjusted")

### FITTING MODELS FOR INVASION 

control.I <- how(within = Within(type = 'none'),
                 plots = Plots(strata = com.dat.invade$plot_type, type = 'free'),
                 nperm = 1000)

permutations.I <- shuffleSet(nrow(com_bund_invade), control = control.I)


# starting with the full model:

invade.m0 <- manyglm(com_bund_invade ~ com.dat.invade$year + com.dat.invade$area + com.dat.invade$plot +
                     com.dat.invade$worm.count + 
                     com.dat.invade$area:com.dat.invade$plot + 
                     com.dat.invade$year:com.dat.invade$area + com.dat.invade$year:com.dat.invade$plot + 
                     com.dat.invade$year:com.dat.invade$area:com.dat.invade$plot)

# checking three-way interaction (Area:Plot:Year)

invade.m1 <- manyglm(com_bund_invade ~ com.dat.invade$year + com.dat.invade$area + com.dat.invade$plot +
                     com.dat.invade$worm.count + 
                     com.dat.invade$area:com.dat.invade$plot + 
                     com.dat.invade$year:com.dat.invade$area + com.dat.invade$year:com.dat.invade$plot)

apy.i <- anova.manyglm(invade.m0, invade.m1, bootID = permutations.I, p.uni = 'adjusted', test = 'LR')

apy.i # dev = 20.34; p = 0.031 *

# three-way interaction came out as significant so I will include all two-way
# interactions and test singular effect of earthworm count

invade.m2 <- manyglm(com_bund_invade ~ com.dat.invade$year + com.dat.invade$area + com.dat.invade$plot +
                      com.dat.invade$area:com.dat.invade$plot + 
                      com.dat.invade$year:com.dat.invade$area + com.dat.invade$year:com.dat.invade$plot + 
                      com.dat.invade$year:com.dat.invade$area:com.dat.invade$plot)

ew.i <- anova.manyglm(invade.m0, invade.m2, bootID = permutations.I, p.uni = 'adjusted', test = 'LR')

ew.i # dev = 481.1; p = 0.001 ***

# earthworm count came out as significant

invade.final <- manyglm(com_bund_invade ~ com.dat.invade$year + com.dat.invade$area + com.dat.invade$plot +
                          com.dat.invade$worm.count + 
                          com.dat.invade$area:com.dat.invade$plot + 
                          com.dat.invade$year:com.dat.invade$area + com.dat.invade$year:com.dat.invade$plot + 
                          com.dat.invade$year:com.dat.invade$area:com.dat.invade$plot)

invade.final.output <- anova.manyglm(invade.final, bootID = permutations.I, p.uni = "adjusted")



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
com.dat2 <- com.dat2[, !(names(com.dat2) %in% c("woody1", "woody2"))]
com.dat.fence2 <- com.dat2[com.dat2$invaded == "inv", ]
com.dat.invade2 <- com.dat2[com.dat2$fencing == "O", ]

com_bund_fence2 <- mvabund(com.dat.fence2[,11:86]) # transforms community data into mvabund appropriate object
com_bund_invade2 <- mvabund(com.dat.invade2[,11:86])

### FITTING MODELS FOR FENCING

control.F2 <- how(within = Within(type = 'none'),
                 plots = Plots(strata = com.dat.fence2$plot_type, type = 'free'),
                 nperm = 1000)

permutations.F2 <- shuffleSet(nrow(com_bund_fence2), control = control.F)

# starting with the full model:

fence.m0.2 <- manyglm(com_bund_fence2 ~ com.dat.fence2$year + com.dat.fence2$area + com.dat.fence2$plot +   # complete model
                      com.dat.fence2$worm.count + 
                      com.dat.fence2$area:com.dat.fence2$plot + 
                      com.dat.fence2$year:com.dat.fence2$area + com.dat.fence2$year:com.dat.fence2$plot + 
                      com.dat.fence2$year:com.dat.fence2$area:com.dat.fence2$plot)

# checking three-way interaction (Area:Plot:Year)

fence.m1.2 <- manyglm(com_bund_fence2 ~ com.dat.fence2$year + com.dat.fence2$area + com.dat.fence2$plot +   # no 3way
                      com.dat.fence2$worm.count + 
                      com.dat.fence2$area:com.dat.fence2$plot + 
                      com.dat.fence2$year:com.dat.fence2$area + com.dat.fence2$year:com.dat.fence2$plot)

apy.f.2 <- anova.manyglm(fence.m0.2, fence.m1.2, bootID = permutations.F2, p.uni = 'adjusted', test = 'LR')

apy.f.2 # dev = 20.75; p = 0.108

# three-way interaction came out as insignificant so I will now test the two-way interactions

# Area:Plot

fence.m2.2 <- manyglm(com_bund_fence2 ~ com.dat.fence2$year + com.dat.fence2$area + 
                      com.dat.fence2$plot + com.dat.fence2$worm.count + 
                      com.dat.fence2$year:com.dat.fence2$area + 
                      com.dat.fence2$year:com.dat.fence2$plot)

ap.f.2 <- anova.manyglm(fence.m1.2, fence.m2.2, bootID = permutations.F2, p.uni = 'adjusted', test = 'LR')

ap.f.2 # dev = 71.27; p = 0.191

# Area:Year

fence.m3.2 <- manyglm(com_bund_fence2 ~ com.dat.fence2$year + com.dat.fence2$area + 
                      com.dat.fence2$plot + com.dat.fence2$worm.count + 
                      com.dat.fence2$area:com.dat.fence2$plot + 
                      com.dat.fence2$year:com.dat.fence2$plot)

ay.f.2 <- anova.manyglm(fence.m1.2, fence.m3.2, bootID = permutations.F2, p.uni = 'adjusted', test = 'LR')

ay.f.2 # dev = 93.2; p = 0.001 ***

# Plot:Year

fence.m4.2 <- manyglm(com_bund_fence2 ~ com.dat.fence2$year + com.dat.fence2$area + 
                      com.dat.fence2$plot + com.dat.fence2$worm.count + 
                      com.dat.fence2$year:com.dat.fence2$area + 
                      com.dat.fence2$area:com.dat.fence2$plot)

py.f.2 <- anova.manyglm(fence.m1.2, fence.m4.2, bootID = permutations.F2, p.uni = 'adjusted', test = 'LR')

py.f.2 # dev = 35.38; p = 0.515

# Area:Year interaction came out as significant but Area:Plot and 
# Plot:Year did not

# now checking non-interactive terms

fence.m5.2 <- manyglm(com_bund_fence2 ~ com.dat.fence2$year + com.dat.fence2$area + 
                      com.dat.fence2$plot + com.dat.fence2$worm.count + 
                      com.dat.fence2$year:com.dat.fence2$area)
# Plot

fence.m6.2 <- manyglm(com_bund_fence2 ~ com.dat.fence2$year + com.dat.fence2$area + 
                      com.dat.fence2$worm.count + com.dat.fence2$year:com.dat.fence2$area)

p.f.2 <- anova.manyglm(fence.m5.2, fence.m6.2, bootID = permutations.F2, p.uni = 'adjusted', test = 'LR')

p.f.2# dev = 258.7; p = 0.004 **

# Earthworms

fence.m7.2 <- manyglm(com_bund_fence2 ~ com.dat.fence2$year + com.dat.fence2$area + 
                      com.dat.fence2$plot + com.dat.fence2$year:com.dat.fence2$area)

ew.f.2 <- anova.manyglm(fence.m5.2, fence.m7.2, bootID = permutations.F2, p.uni = 'adjusted', test = 'LR')

ew.f.2 # dev = 358.4; p = 0.001 ***

# both plot and earthworm count came out as significant

fence.final.2 <- manyglm(com_bund_fence2 ~ com.dat.fence2$year + com.dat.fence2$area + 
                        com.dat.fence2$plot + com.dat.fence2$worm.count + 
                        com.dat.fence2$year:com.dat.fence2$area)

fence.final.2.output <- anova.manyglm(fence.final.2, bootID = permutations.F2, p.uni = "adjusted")


### FITTING MODELS FOR INVASION 

control.I2 <- how(within = Within(type = 'none'),
                  plots = Plots(strata = com.dat.invade2$plot_type, type = 'free'),
                  nperm = 1000)

permutations.I2 <- shuffleSet(nrow(com_bund_invade2), control = control.I2)

# starting with the full model:

invade.m0.2 <- manyglm(com_bund_invade2 ~ com.dat.invade2$year + com.dat.invade2$area + com.dat.invade2$plot +
                       com.dat.invade2$worm.count + 
                       com.dat.invade2$area:com.dat.invade2$plot + 
                       com.dat.invade2$year:com.dat.invade2$area + com.dat.invade2$year:com.dat.invade2$plot + 
                       com.dat.invade2$year:com.dat.invade2$area:com.dat.invade2$plot)

# checking three-way interaction (Area:Plot:Year)

invade.m1.2 <- manyglm(com_bund_invade2 ~ com.dat.invade2$year + com.dat.invade2$area + com.dat.invade2$plot +
                       com.dat.invade2$worm.count + 
                       com.dat.invade2$area:com.dat.invade2$plot + 
                       com.dat.invade2$year:com.dat.invade2$area + com.dat.invade2$year:com.dat.invade2$plot)

apy.i.2 <- anova.manyglm(invade.m0.2, invade.m1.2, bootID = permutations.I2, p.uni = 'adjusted', test = 'LR')

apy.i.2 # dev = 19.35; p = 0.024 * 

# three-way interaction came out as significant so I2 will include all two-way
# interactions and test singular effect of earthworm count

invade.m2.2 <- manyglm(com_bund_invade2 ~ com.dat.invade2$year + com.dat.invade2$area + com.dat.invade2$plot +
                       com.dat.invade2$area:com.dat.invade2$plot + 
                       com.dat.invade2$year:com.dat.invade2$area + com.dat.invade2$year:com.dat.invade2$plot + 
                       com.dat.invade2$year:com.dat.invade2$area:com.dat.invade2$plot)

ew.i.2 <- anova.manyglm(invade.m0.2, invade.m2.2, bootID = permutations.I2, p.uni = 'adjusted', test = 'LR')

ew.i.2 # dev = 453.1; p = 0.001 ***

# earthworm count came out as significant

invade.final.2 <- manyglm(com_bund_invade2 ~ com.dat.invade2$year + com.dat.invade2$area + com.dat.invade2$plot +
                          com.dat.invade2$worm.count + 
                          com.dat.invade2$area:com.dat.invade2$plot + 
                          com.dat.invade2$year:com.dat.invade2$area + com.dat.invade2$year:com.dat.invade2$plot + 
                          com.dat.invade2$year:com.dat.invade2$area:com.dat.invade2$plot)

invade.final.2.output <- anova.manyglm(invade.final.2, bootID = permutations.I2, p.uni = "adjusted")


##### III. NATIVE ONLY

### PREPARING DATA FOR MVABUND

com.dat3 <- cbind.data.frame("site" = mydata$identifiers$site, "area" = mydata$identifiers$area, 
                             "plot" = mydata$identifiers$plot, "plot_type" = plot_type, 
                             "fencing" = fencing, "invaded" = invaded,
                             "year" = year, "worm.mass" = worm.mass, "worm.count" = worm.count,
                             "psw" = psw, round(natives))
com.dat3 <- com.dat3[, !(names(com.dat3) %in% c("woody1", "woody2"))]
com.dat.fence3 <- com.dat3[com.dat3$invaded == "inv", ]
com.dat.invade3 <- com.dat3[com.dat3$fencing == "O", ]

com_bund_fence3 <- mvabund(com.dat.fence3[,11:65]) # transforms community data into mvabund appropriate object
com_bund_invade3 <- mvabund(com.dat.invade3[,11:65])

### FITTING MODELS FOR FENCING

control.F3 <- how(within = Within(type = 'none'),
                  plots = Plots(strata = com.dat.fence3$plot_type, type = 'free'),
                  nperm = 1000)

permutations.F3 <- shuffleSet(nrow(com_bund_fence3), control = control.F3)


# starting with the full model:

fence.m0.3 <- manyglm(com_bund_fence3 ~ com.dat.fence3$year + com.dat.fence3$area + com.dat.fence3$plot +   # complete model
                        com.dat.fence3$worm.count + 
                        com.dat.fence3$area:com.dat.fence3$plot + 
                        com.dat.fence3$year:com.dat.fence3$area + com.dat.fence3$year:com.dat.fence3$plot + 
                        com.dat.fence3$year:com.dat.fence3$area:com.dat.fence3$plot)

# checking three-way interaction (Area:Plot:Year)

fence.m1.3 <- manyglm(com_bund_fence3 ~ com.dat.fence3$year + com.dat.fence3$area + com.dat.fence3$plot +   # no 3way
                        com.dat.fence3$worm.count + 
                        com.dat.fence3$area:com.dat.fence3$plot + 
                        com.dat.fence3$year:com.dat.fence3$area + com.dat.fence3$year:com.dat.fence3$plot)

apy.f.3 <- anova.manyglm(fence.m0.3, fence.m1.3, bootID = permutations.F3, p.uni = 'adjusted', test = 'LR')

apy.f.3 # dev = 7.617; p = 0.435

# three-way interaction came out as insignificant so I will now test the two-way interactions

# Area:Plot

fence.m2.3 <- manyglm(com_bund_fence3 ~ com.dat.fence3$year + com.dat.fence3$area + 
                        com.dat.fence3$plot + com.dat.fence3$worm.count + 
                        com.dat.fence3$year:com.dat.fence3$area + 
                        com.dat.fence3$year:com.dat.fence3$plot)

ap.f.3 <- anova.manyglm(fence.m1.3, fence.m2.3, bootID = permutations.F3, p.uni = 'adjusted', test = 'LR')

ap.f.3 # dev = 54.38; p = 0.138

# Area:Year

fence.m3.3 <- manyglm(com_bund_fence3 ~ com.dat.fence3$year + com.dat.fence3$area + 
                        com.dat.fence3$plot + com.dat.fence3$worm.count + 
                        com.dat.fence3$area:com.dat.fence3$plot + 
                        com.dat.fence3$year:com.dat.fence3$plot)

ay.f.3 <- anova.manyglm(fence.m1.3, fence.m3.3, bootID = permutations.F3, p.uni = 'adjusted', test = 'LR')

ay.f.3 # dev = 73.17; p = 0.001 ***

# Plot:Year

fence.m4.3 <- manyglm(com_bund_fence3 ~ com.dat.fence3$year + com.dat.fence3$area + 
                        com.dat.fence3$plot + com.dat.fence3$worm.count + 
                        com.dat.fence3$year:com.dat.fence3$area + 
                        com.dat.fence3$area:com.dat.fence3$plot)

py.f.3 <- anova.manyglm(fence.m1.3, fence.m4.3, bootID = permutations.F3, p.uni = 'adjusted', test = 'LR')

py.f.3 # dev = 27.21; p = 0.524

# Area:Year interaction came out as significant but Area:Plot and 
# Plot:Year did not

# now checking non-interactive terms

fence.m5.3 <- manyglm(com_bund_fence3 ~ com.dat.fence3$year + com.dat.fence3$area + 
                        com.dat.fence3$plot + com.dat.fence3$worm.count + 
                        com.dat.fence3$year:com.dat.fence3$area)
# Plot

fence.m6.3 <- manyglm(com_bund_fence3 ~ com.dat.fence3$year + com.dat.fence3$area + 
                        com.dat.fence3$worm.count + com.dat.fence3$year:com.dat.fence3$area)

p.f.3 <- anova.manyglm(fence.m5.3, fence.m6.3, bootID = permutations.F3, p.uni = 'adjusted', test = 'LR')

p.f.3 # dev = 186.1; p = 0.002 **

# Earthworms

fence.m7.3 <- manyglm(com_bund_fence3 ~ com.dat.fence3$year + com.dat.fence3$area + 
                        com.dat.fence3$plot + com.dat.fence3$year:com.dat.fence3$area)

ew.f.3 <- anova.manyglm(fence.m5.3, fence.m7.3, bootID = permutations.F3, p.uni = 'adjusted', test = 'LR')

ew.f.3 # dev = 288.2; p = 0.001 ***

# both plot and earthworm count came out as significant

fence.final.3 <- manyglm(com_bund_fence3 ~ com.dat.fence3$year + com.dat.fence3$area + 
                           com.dat.fence3$plot + com.dat.fence3$worm.count + 
                           com.dat.fence3$year:com.dat.fence3$area)

fence.final.3.output <- anova.manyglm(fence.final.3, bootID = permutations.F3, p.uni = "adjusted")

### FITTING MODELS FOR INVASION 

control.I3 <- how(within = Within(type = 'none'),
                  plots = Plots(strata = com.dat.invade3$plot_type, type = 'free'),
                  nperm = 1000)

permutations.I3 <- shuffleSet(nrow(com_bund_invade3), control = control.I3)

# starting with the full model:

invade.m0.3 <- manyglm(com_bund_invade3 ~ com.dat.invade3$year + com.dat.invade3$area + com.dat.invade3$plot +
                         com.dat.invade3$worm.count + 
                         com.dat.invade3$area:com.dat.invade3$plot + 
                         com.dat.invade3$year:com.dat.invade3$area + com.dat.invade3$year:com.dat.invade3$plot + 
                         com.dat.invade3$year:com.dat.invade3$area:com.dat.invade3$plot)

# checking three-way interaction (Area:Plot:Year)

invade.m1.3 <- manyglm(com_bund_invade3 ~ com.dat.invade3$year + com.dat.invade3$area + com.dat.invade3$plot +
                         com.dat.invade3$worm.count + 
                         com.dat.invade3$area:com.dat.invade3$plot + 
                         com.dat.invade3$year:com.dat.invade3$area + com.dat.invade3$year:com.dat.invade3$plot)

apy.i.3 <- anova.manyglm(invade.m0.3, invade.m1.3, bootID = permutations.I3, p.uni = 'adjusted', test = 'LR')

apy.i.3 # dev = 17.38; p = 0.032

# three-way interaction came out as significant so I3 will include all two-way
# interactions and test singular effect of earthworm count

invade.m2.3 <- manyglm(com_bund_invade3 ~ com.dat.invade3$year + com.dat.invade3$area + com.dat.invade3$plot +
                         com.dat.invade3$area:com.dat.invade3$plot + 
                         com.dat.invade3$year:com.dat.invade3$area + com.dat.invade3$year:com.dat.invade3$plot + 
                         com.dat.invade3$year:com.dat.invade3$area:com.dat.invade3$plot)

ew.i.3 <- anova.manyglm(invade.m0.3, invade.m2.3, bootID = permutations.I3, p.uni = 'adjusted', test = 'LR')

ew.i.3 # dev = 382.8; p = 0.001

# earthworm count came out as significant

invade.final.3 <- manyglm(com_bund_invade3 ~ com.dat.invade3$year + com.dat.invade3$area + com.dat.invade3$plot +
                            com.dat.invade3$worm.count + 
                            com.dat.invade3$area:com.dat.invade3$plot + 
                            com.dat.invade3$year:com.dat.invade3$area + com.dat.invade3$year:com.dat.invade3$plot + 
                            com.dat.invade3$year:com.dat.invade3$area:com.dat.invade3$plot)

invade.final.3.output <- anova.manyglm(invade.final.3, bootID = permutations.I3, p.uni = "adjusted")


##### IV. PIE CHARTS

fence.final
