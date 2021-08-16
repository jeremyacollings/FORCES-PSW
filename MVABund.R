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

### FITTING MODELS FOR FENCING

trym.f0 <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area + com.dat.fence$plot +   # complete model
                     com.dat.fence$worm.count + 
                     com.dat.fence$area:com.dat.fence$plot + 
                     com.dat.fence$year:com.dat.fence$area + com.dat.fence$year:com.dat.fence$plot + 
                     com.dat.fence$year:com.dat.fence$area:com.dat.fence$plot)

trym.f1 <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area + com.dat.fence$plot +   # no 3way
                     com.dat.fence$worm.count + 
                     com.dat.fence$area:com.dat.fence$plot + 
                     com.dat.fence$year:com.dat.fence$area + com.dat.fence$year:com.dat.fence$plot)

trym.f2 <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area + com.dat.fence$plot +   # no year:plot
                     com.dat.fence$worm.count + 
                     com.dat.fence$area:com.dat.fence$plot + 
                     com.dat.fence$year:com.dat.fence$area)

trym.f3 <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area + com.dat.fence$plot +   # no year:area
                     com.dat.fence$worm.count + 
                     com.dat.fence$area:com.dat.fence$plot + 
                     com.dat.fence$year:com.dat.fence$plot)

trym.f4 <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area + com.dat.fence$plot +   # no ara:plot
                     com.dat.fence$worm.count +
                     com.dat.fence$year:com.dat.fence$area + com.dat.fence$year:com.dat.fence$plot)

trym.f5 <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area + com.dat.fence$plot +    # no worms
                     com.dat.fence$area:com.dat.fence$plot + 
                     com.dat.fence$year:com.dat.fence$area + com.dat.fence$year:com.dat.fence$plot + 
                     com.dat.fence$year:com.dat.fence$area:com.dat.fence$plot)

trym.f6 <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area +    # no plot
                     com.dat.fence$worm.count + 
                     com.dat.fence$year:com.dat.fence$area)

trym.f7 <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$plot +   # no area
                     com.dat.fence$worm.count + com.dat.fence$year:com.dat.fence$plot)

trym.f8 <- manyglm(com_bund_fence ~ com.dat.fence$area + com.dat.fence$plot +   # no year
                     com.dat.fence$worm.count + 
                     com.dat.fence$area:com.dat.fence$plot)

control.F <- how(within = Within(type = 'none'),
                 plots = Plots(strata = com.dat.fence$plot_type, type = 'free'),
                 nperm = 1000)

permutations.F <- shuffleSet(nrow(com_bund_fence), control = control.F)

YPA.F <- anova(trym.f0, trym.f1, bootID = permutations.F, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.F, "Whole YPA Fenced.rds")
YP.F <- anova(trym.f0, trym.f2, bootID = permutations.F, p.uni = 'adjusted', test = 'LR')
saveRDS(YP.F, "Whole YP Fenced.rds")
YA.F <- anova(trym.f0, trym.f3, bootID = permutations.F, p.uni = 'adjusted', test = 'LR')
saveRDS(YA.F, "Whole YA Fenced.rds")
AP.F <- anova(trym.f0, trym.f4, bootID = permutations.F, p.uni = 'adjusted', test = 'LR')
saveRDS(AP.F, "Whole AP Fenced.rds")
worms.F <- anova(trym.f0, trym.f5, bootID = permutations.F, p.uni = 'adjusted', test = 'LR')
saveRDS(worms.F, "Whole Worms Fenced.rds")
plot.F <- anova(trym.f0, trym.f6, bootID = permutations.F, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.F, "Whole Plot Fenced.rds")
area.F <- anova(trym.f0, trym.f7, bootID = permutations.F, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.F, "Whole Area Fenced.rds")
year.F <- anova(trym.f0, trym.f8, bootID = permutations.F, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.F, "Whole Year Fenced.rds")

m.f.final <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area +
                       com.dat.fence$plot + com.dat.fence$worm.count + 
                       com.dat.fence$year:com.dat.fence$area)

### FITTING MODELS FOR INVASION 

control.I <- how(within = Within(type = 'none'),
                 plots = Plots(strata = com.dat.invade$plot_type, type = 'free'),
                 nperm = 1000)

permutations.I <- shuffleSet(nrow(com_bund_invade), control = control.I)

trym.i0 <- manyglm(com_bund_invade ~ com.dat.invade$year + com.dat.invade$area + com.dat.invade$plot +   # complete model
                     com.dat.invade$worm.count + 
                     com.dat.invade$area:com.dat.invade$plot + 
                     com.dat.invade$year:com.dat.invade$area + com.dat.invade$year:com.dat.invade$plot + 
                     com.dat.invade$year:com.dat.invade$area:com.dat.invade$plot)

trym.i1 <- manyglm(com_bund_invade ~ com.dat.invade$year + com.dat.invade$area + com.dat.invade$plot +   # no 3way
                     com.dat.invade$worm.count + 
                     com.dat.invade$area:com.dat.invade$plot + 
                     com.dat.invade$year:com.dat.invade$area + com.dat.invade$year:com.dat.invade$plot)

trym.i2 <- manyglm(com_bund_invade ~ com.dat.invade$year + com.dat.invade$area + com.dat.invade$plot +   # no year:plot
                     com.dat.invade$worm.count + 
                     com.dat.invade$area:com.dat.invade$plot + 
                     com.dat.invade$year:com.dat.invade$area)

trym.i3 <- manyglm(com_bund_invade ~ com.dat.invade$year + com.dat.invade$area + com.dat.invade$plot +   # no year:area
                     com.dat.invade$worm.count + 
                     com.dat.invade$area:com.dat.invade$plot + 
                     com.dat.invade$year:com.dat.invade$plot)

trym.i4 <- manyglm(com_bund_invade ~ com.dat.invade$year + com.dat.invade$area + com.dat.invade$plot +   # no ara:plot
                     com.dat.invade$worm.count +
                     com.dat.invade$year:com.dat.invade$area + com.dat.invade$year:com.dat.invade$plot)

trym.i5 <- manyglm(com_bund_invade ~ com.dat.invade$year + com.dat.invade$area + com.dat.invade$plot +    # no worms
                     com.dat.invade$area:com.dat.invade$plot + 
                     com.dat.invade$year:com.dat.invade$area + com.dat.invade$year:com.dat.invade$plot + 
                     com.dat.invade$year:com.dat.invade$area:com.dat.invade$plot)

trym.i6 <- manyglm(com_bund_invade ~ com.dat.invade$year + com.dat.invade$area +    # no plot
                     com.dat.invade$worm.count + 
                     com.dat.invade$year:com.dat.invade$area)

trym.i7 <- manyglm(com_bund_invade ~ com.dat.invade$year + com.dat.invade$plot +   # no area
                     com.dat.invade$worm.count + com.dat.invade$year:com.dat.invade$plot)

trym.i8 <- manyglm(com_bund_invade ~ com.dat.invade$area + com.dat.invade$plot +   # no year
                     com.dat.invade$worm.count + 
                     com.dat.invade$area:com.dat.invade$plot)

YPA.I <- anova(trym.i0, trym.i1, bootID = permutations.I, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.I, "Whole YPA Invaded.rds")
YP.I <- anova(trym.i0, trym.i2, bootID = permutations.I, p.uni = 'adjusted', test = 'LR')
saveRDS(YP.I, "Whole YP Invaded.rds")
YA.I <- anova(trym.i0, trym.i3, bootID = permutations.I, p.uni = 'adjusted', test = 'LR')
saveRDS(YA.I, "Whole YA Invaded.rds")
AP.I <- anova(trym.i0, trym.i4, bootID = permutations.I, p.uni = 'adjusted', test = 'LR')
saveRDS(AP.I, "Whole AP Invaded.rds")
worms.I <- anova(trym.i0, trym.i5, bootID = permutations.I, p.uni = 'adjusted', test = 'LR')
saveRDS(worms.I, "Whole Worms Invaded.rds")
plot.I <- anova(trym.i0, trym.i6, bootID = permutations.I, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.I, "Whole Plot Invaded.rds")
area.I <- anova(trym.i0, trym.i7, bootID = permutations.I, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.I, "Whole Area Invaded.rds")
year.I <- anova(trym.i0, trym.i8, bootID = permutations.I, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.I, "Whole Year Invaded.rds")

final.f <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area + com.dat.fence$plot +   # complete model
                     com.dat.fence$worm.count +
                     com.dat.fence$year:com.dat.fence$area)

final.i <- manyglm(com_bund_invade ~ com.dat.invade$year + com.dat.invade$area + com.dat.invade$plot +   # complete model
                     com.dat.invade$worm.count + 
                     com.dat.invade$area:com.dat.invade$plot + 
                     com.dat.invade$year:com.dat.invade$area + com.dat.invade$year:com.dat.invade$plot + 
                     com.dat.invade$year:com.dat.invade$area:com.dat.invade$plot)

final.output.F <- anova(final.f, bootID = permutations.F, p.uni = "adjusted")
final.output.I <- anova(final.i, bootID = permutations.I, p.uni = "adjusted")
saveRDS(final.output.F, "MVABund Output Fencing Comparison")
saveRDS(final.output.I, "MVABund Output Invasion Comparison")

## Exploring Deviance Values

tests.F <- as.data.frame(final.output.F[["uni.test"]])
rowSums(tests.F[which(colnames(tests.F) %in% forbspp)])/rowSums(tests.F)
rowSums(tests.F[which(colnames(tests.F) %in% gramspp)])/rowSums(tests.F)
rowSums(tests.F[which(colnames(tests.F) %in% woodyspp)])/rowSums(tests.F)

x <- as.data.frame(c(mydata[["identifiers"]], mydata[["veg_calc"]], mydata[["worms"]]))

ggplot(data = x, aes(x = year, y = sumwoody, fill = area)) + geom_bar(position = "dodge", stat = "summary", fun = "mean")
ggplot(data = x, aes(x = year, y = sumforb, fill = area)) + geom_bar(position = "dodge", stat = "summary", fun = "mean")
ggplot(data = x, aes(x = year, y = sumgram, fill = area)) + geom_bar(position = "dodge", stat = "summary", fun = "mean")

ggplot(data = x, aes(x = ew.count.mean, y = sumwoody)) + geom_point() + geom_smooth()
ggplot(data = x, aes(x = ew.count.mean, y = sumforb)) + geom_point() + geom_smooth()
ggplot(data = x, aes(x = ew.count.mean, y = sumgram)) + geom_point() + geom_smooth()

tests.I <- as.data.frame(final.output.I[["uni.test"]])
rowSums(tests.I[which(colnames(tests.I) %in% forbspp)])/rowSums(tests.I)
rowSums(tests.I[which(colnames(tests.I) %in% gramspp)])/rowSums(tests.I)
rowSums(tests.I[which(colnames(tests.I) %in% woodyspp)])/rowSums(tests.I)









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

com.dat.fence2 <- com.dat2[com.dat2$invaded == "inv", ]
com.dat.invade2 <- com.dat2[com.dat2$fencing == "O", ]

com_bund_fence2 <- mvabund(com.dat.fence2[,11:88]) # transforms community data into mvabund appropriate object
com_bund_invade2 <- mvabund(com.dat.invade2[,11:88])

### FITTING MODELS FOR FENCING

trym2.f0 <- manyglm(com_bund_fence2 ~ com.dat.fence2$year + com.dat.fence2$area + com.dat.fence2$plot +   # complete model
                     com.dat.fence2$worm.count + 
                     com.dat.fence2$area:com.dat.fence2$plot + 
                     com.dat.fence2$year:com.dat.fence2$area + com.dat.fence2$year:com.dat.fence2$plot + 
                     com.dat.fence2$year:com.dat.fence2$area:com.dat.fence2$plot)

trym2.f1 <- manyglm(com_bund_fence2 ~ com.dat.fence2$year + com.dat.fence2$area + com.dat.fence2$plot +   # no 3way
                     com.dat.fence2$worm.count + 
                     com.dat.fence2$area:com.dat.fence2$plot + 
                     com.dat.fence2$year:com.dat.fence2$area + com.dat.fence2$year:com.dat.fence2$plot)

trym2.f2 <- manyglm(com_bund_fence2 ~ com.dat.fence2$year + com.dat.fence2$area + com.dat.fence2$plot +   # no year:plot
                     com.dat.fence2$worm.count + 
                     com.dat.fence2$area:com.dat.fence2$plot + 
                     com.dat.fence2$year:com.dat.fence2$area)

trym2.f3 <- manyglm(com_bund_fence2 ~ com.dat.fence2$year + com.dat.fence2$area + com.dat.fence2$plot +   # no year:area
                     com.dat.fence2$worm.count + 
                     com.dat.fence2$area:com.dat.fence2$plot + 
                     com.dat.fence2$year:com.dat.fence2$plot)

trym2.f4 <- manyglm(com_bund_fence2 ~ com.dat.fence2$year + com.dat.fence2$area + com.dat.fence2$plot +   # no ara:plot
                     com.dat.fence2$worm.count +
                     com.dat.fence2$year:com.dat.fence2$area + com.dat.fence2$year:com.dat.fence2$plot)

trym2.f5 <- manyglm(com_bund_fence2 ~ com.dat.fence2$year + com.dat.fence2$area + com.dat.fence2$plot +    # no worms
                     com.dat.fence2$area:com.dat.fence2$plot + 
                     com.dat.fence2$year:com.dat.fence2$area + com.dat.fence2$year:com.dat.fence2$plot + 
                     com.dat.fence2$year:com.dat.fence2$area:com.dat.fence2$plot)

trym2.f6 <- manyglm(com_bund_fence2 ~ com.dat.fence2$year + com.dat.fence2$area +    # no plot
                     com.dat.fence2$worm.count + 
                     com.dat.fence2$year:com.dat.fence2$area)

trym2.f7 <- manyglm(com_bund_fence2 ~ com.dat.fence2$year + com.dat.fence2$plot +   # no area
                     com.dat.fence2$worm.count + com.dat.fence2$year:com.dat.fence2$plot)

trym2.f8 <- manyglm(com_bund_fence2 ~ com.dat.fence2$area + com.dat.fence2$plot +   # no year
                     com.dat.fence2$worm.count + 
                     com.dat.fence2$area:com.dat.fence2$plot)

control.F2 <- how(within = Within(type = 'none'),
                 plots = Plots(strata = com.dat.fence2$plot_type, type = 'free'),
                 nperm = 1000)

permutations.F2 <- shuffleSet(nrow(com_bund_fence2), control = control.F2)

YPA.F2 <- anova(trym2.f0, trym2.f1, bootID = permutations.F2, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.F, "Non-Invasive YPA Fenced.rds")
YP.F2 <- anova(trym2.f0, trym2.f2, bootID = permutations.F2, p.uni = 'adjusted', test = 'LR')
saveRDS(YP.F, "Non-Invasive YP Fenced.rds")
YA.F2 <- anova(trym2.f0, trym2.f3, bootID = permutations.F2, p.uni = 'adjusted', test = 'LR')
saveRDS(YA.F, "Non-Invasive YA Fenced.rds")
AP.F2 <- anova(trym2.f0, trym2.f4, bootID = permutations.F2, p.uni = 'adjusted', test = 'LR')
saveRDS(AP.F, "Non-Invasive AP Fenced.rds")
worms.F2 <- anova(trym2.f0, trym2.f5, bootID = permutations.F2, p.uni = 'adjusted', test = 'LR')
saveRDS(worms.F, "Non-Invasive Worms Fenced.rds")
plot.F2 <- anova(trym2.f0, trym2.f6, bootID = permutations.F2, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.F, "Non-Invasive Plot Fenced.rds")
area.F2 <- anova(trym2.f0, trym2.f7, bootID = permutations.F2, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.F, "Non-Invasive Area Fenced.rds")
year.F2 <- anova(trym2.f0, trym2.f8, bootID = permutations.F2, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.F, "Non-Invasive Year Fenced.rds")


m.f.final2 <- manyglm(com_bund_fence2 ~ com.dat.fence2$year:com.dat.fence2$area + com.dat.fence2$worm.count +
                        com.dat.fence2$plot + com.dat.fence2$area + com.dat.fence2$year)

### FITTING MODELS FOR INVASION 

control.I2 <- how(within = Within(type = 'none'),
                 plots = Plots(strata = com.dat.invade2$plot_type, type = 'free'),
                 nperm = 1000)

permutations.I2 <- shuffleSet(nrow(com_bund_invade2), control = control.I2)

trym2.i0 <- manyglm(com_bund_invade2 ~ com.dat.invade2$year + com.dat.invade2$area + com.dat.invade2$plot +   # complete model
                     com.dat.invade2$worm.count + 
                     com.dat.invade2$area:com.dat.invade2$plot + 
                     com.dat.invade2$year:com.dat.invade2$area + com.dat.invade2$year:com.dat.invade2$plot + 
                     com.dat.invade2$year:com.dat.invade2$area:com.dat.invade2$plot)

trym2.i1 <- manyglm(com_bund_invade2 ~ com.dat.invade2$year + com.dat.invade2$area + com.dat.invade2$plot +   # no 3way
                     com.dat.invade2$worm.count + 
                     com.dat.invade2$area:com.dat.invade2$plot + 
                     com.dat.invade2$year:com.dat.invade2$area + com.dat.invade2$year:com.dat.invade2$plot)

trym2.i2 <- manyglm(com_bund_invade2 ~ com.dat.invade2$year + com.dat.invade2$area + com.dat.invade2$plot +   # no year:plot
                     com.dat.invade2$worm.count + 
                     com.dat.invade2$area:com.dat.invade2$plot + 
                     com.dat.invade2$year:com.dat.invade2$area)

trym2.i3 <- manyglm(com_bund_invade2 ~ com.dat.invade2$year + com.dat.invade2$area + com.dat.invade2$plot +   # no year:area
                     com.dat.invade2$worm.count + 
                     com.dat.invade2$area:com.dat.invade2$plot + 
                     com.dat.invade2$year:com.dat.invade2$plot)

trym2.i4 <- manyglm(com_bund_invade2 ~ com.dat.invade2$year + com.dat.invade2$area + com.dat.invade2$plot +   # no ara:plot
                     com.dat.invade2$worm.count +
                     com.dat.invade2$year:com.dat.invade2$area + com.dat.invade2$year:com.dat.invade2$plot)

trym2.i5 <- manyglm(com_bund_invade2 ~ com.dat.invade2$year + com.dat.invade2$area + com.dat.invade2$plot +    # no worms
                     com.dat.invade2$area:com.dat.invade2$plot + 
                     com.dat.invade2$year:com.dat.invade2$area + com.dat.invade2$year:com.dat.invade2$plot + 
                     com.dat.invade2$year:com.dat.invade2$area:com.dat.invade2$plot)

trym2.i6 <- manyglm(com_bund_invade2 ~ com.dat.invade2$year + com.dat.invade2$area +    # no plot
                     com.dat.invade2$worm.count + 
                     com.dat.invade2$year:com.dat.invade2$area)

trym2.i7 <- manyglm(com_bund_invade2 ~ com.dat.invade2$year + com.dat.invade2$plot +   # no area
                     com.dat.invade2$worm.count + com.dat.invade2$year:com.dat.invade2$plot)

trym2.i8 <- manyglm(com_bund_invade2 ~ com.dat.invade2$area + com.dat.invade2$plot +   # no year
                     com.dat.invade2$worm.count + 
                     com.dat.invade2$area:com.dat.invade2$plot)

YPA.I2 <- anova(trym2.i0, trym2.i1, bootID = permutations.I2, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.I, "Non-Invasive YPA Invaded.rds")
YP.I2 <- anova(trym2.i0, trym2.i2, bootID = permutations.I2, p.uni = 'adjusted', test = 'LR')
saveRDS(YP.I, "Non-Invasive YP Invaded.rds")
YA.I2 <- anova(trym2.i0, trym2.i3, bootID = permutations.I2, p.uni = 'adjusted', test = 'LR')
saveRDS(YA.I, "Non-Invasive YA Invaded.rds")
AP.I2 <- anova(trym2.i0, trym2.i4, bootID = permutations.I2, p.uni = 'adjusted', test = 'LR')
saveRDS(AP.I, "Non-Invasive AP Invaded.rds")
worms.I2 <- anova(trym2.i0, trym2.i5, bootID = permutations.I2, p.uni = 'adjusted', test = 'LR')
saveRDS(worms.I, "Non-Invasive Worms Invaded.rds")
plot.I2 <- anova(trym2.i0, trym2.i6, bootID = permutations.I2, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.I, "Non-Invasive Plot Invaded.rds")
area.I2 <- anova(trym2.i0, trym2.i7, bootID = permutations.I2, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.I, "Non-Invasive Area Invaded.rds")
year.I2 <- anova(trym2.i0, trym2.i8, bootID = permutations.I2, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.I, "Non-Invasive Year Invaded.rds")

final.f2 <- manyglm(com_bund_fence ~ com.dat.fence$year + com.dat.fence$area + com.dat.fence$plot +   # complete model
                     com.dat.fence$worm.count +
                     com.dat.fence$year:com.dat.fence$area)

final.i2 <- manyglm(com_bund_invade2 ~ com.dat.invade2$year + com.dat.invade2$area + com.dat.invade2$plot +   # complete model
                     com.dat.invade2$worm.count + 
                     com.dat.invade2$area:com.dat.invade2$plot + 
                     com.dat.invade2$year:com.dat.invade2$area + com.dat.invade2$year:com.dat.invade2$plot + 
                     com.dat.invade2$year:com.dat.invade2$area:com.dat.invade2$plot)

final.output.F2 <- anova(final.f2, bootID = permutations.F2, p.uni = "adjusted")
final.output.I2 <- anova(final.i2, bootID = permutations.I2, p.uni = "adjusted")
saveRDS(final.output.F2, "MVABund Output Fencing Comparison Non-Invasive")
saveRDS(final.output.I2, "MVABund Output Invasion Comparison Non-Invasive")

## Exploring Deviance Values

#tests.F <- as.data.frame(final.output.F[["uni.test"]])
#rowSums(tests.F[which(colnames(tests.F) %in% forbspp)])/rowSums(tests.F)
#rowSums(tests.F[which(colnames(tests.F) %in% gramspp)])/rowSums(tests.F)
#rowSums(tests.F[which(colnames(tests.F) %in% woodyspp)])/rowSums(tests.F)

#x <- as.data.frame(c(mydata[["identifiers"]], mydata[["veg_calc"]], mydata[["worms"]]))

#ggplot(data = x, aes(x = year, y = sumwoody, fill = area)) + geom_bar(position = "dodge", stat = "summary", fun = "mean")
#ggplot(data = x, aes(x = year, y = sumforb, fill = area)) + geom_bar(position = "dodge", stat = "summary", fun = "mean")
#ggplot(data = x, aes(x = year, y = sumgram, fill = area)) + geom_bar(position = "dodge", stat = "summary", fun = "mean")

#ggplot(data = x, aes(x = ew.count.mean, y = sumwoody)) + geom_point() + geom_smooth()
#ggplot(data = x, aes(x = ew.count.mean, y = sumforb)) + geom_point() + geom_smooth()
#ggplot(data = x, aes(x = ew.count.mean, y = sumgram)) + geom_point() + geom_smooth()

#tests.I <- as.data.frame(final.output.I[["uni.test"]])
#rowSums(tests.I[which(colnames(tests.I) %in% forbspp)])/rowSums(tests.I)
#rowSums(tests.I[which(colnames(tests.I) %in% gramspp)])/rowSums(tests.I)
#rowSums(tests.I[which(colnames(tests.I) %in% woodyspp)])/rowSums(tests.I)




##### II. NATIVE ONLY

### PREPARING DATA FOR MVABUND

com.dat3 <- cbind.data.frame("site" = mydata$identifiers$site, "area" = mydata$identifiers$area, 
                             "plot" = mydata$identifiers$plot, "plot_type" = plot_type, 
                             "fencing" = fencing, "invaded" = invaded,
                             "year" = year, "worm.mass" = worm.mass, "worm.count" = worm.count,
                             "psw" = psw, round(natives))

com.dat.fence3 <- com.dat3[com.dat3$invaded == "inv", ]
com.dat.invade3 <- com.dat3[com.dat3$fencing == "O", ]

com_bund_fence3 <- mvabund(com.dat.fence3[,11:65]) # transforms community data into mvabund appropriate object
com_bund_invade3 <- mvabund(com.dat.invade3[,11:65])

### FITTING MODELS FOR FENCING

trym3.f0 <- manyglm(com_bund_fence3 ~ com.dat.fence3$year + com.dat.fence3$area + com.dat.fence3$plot +   # complete model
                      com.dat.fence3$worm.count + 
                      com.dat.fence3$area:com.dat.fence3$plot + 
                      com.dat.fence3$year:com.dat.fence3$area + com.dat.fence3$year:com.dat.fence3$plot + 
                      com.dat.fence3$year:com.dat.fence3$area:com.dat.fence3$plot)

trym3.f1 <- manyglm(com_bund_fence3 ~ com.dat.fence3$year + com.dat.fence3$area + com.dat.fence3$plot +   # no 3way
                      com.dat.fence3$worm.count + 
                      com.dat.fence3$area:com.dat.fence3$plot + 
                      com.dat.fence3$year:com.dat.fence3$area + com.dat.fence3$year:com.dat.fence3$plot)

trym3.f2 <- manyglm(com_bund_fence3 ~ com.dat.fence3$year + com.dat.fence3$area + com.dat.fence3$plot +   # no year:plot
                      com.dat.fence3$worm.count + 
                      com.dat.fence3$area:com.dat.fence3$plot + 
                      com.dat.fence3$year:com.dat.fence3$area)

trym3.f3 <- manyglm(com_bund_fence3 ~ com.dat.fence3$year + com.dat.fence3$area + com.dat.fence3$plot +   # no year:area
                      com.dat.fence3$worm.count + 
                      com.dat.fence3$area:com.dat.fence3$plot + 
                      com.dat.fence3$year:com.dat.fence3$plot)

trym3.f4 <- manyglm(com_bund_fence3 ~ com.dat.fence3$year + com.dat.fence3$area + com.dat.fence3$plot +   # no ara:plot
                      com.dat.fence3$worm.count +
                      com.dat.fence3$year:com.dat.fence3$area + com.dat.fence3$year:com.dat.fence3$plot)

trym3.f5 <- manyglm(com_bund_fence3 ~ com.dat.fence3$year + com.dat.fence3$area + com.dat.fence3$plot +    # no worms
                      com.dat.fence3$area:com.dat.fence3$plot + 
                      com.dat.fence3$year:com.dat.fence3$area + com.dat.fence3$year:com.dat.fence3$plot + 
                      com.dat.fence3$year:com.dat.fence3$area:com.dat.fence3$plot)

trym3.f6 <- manyglm(com_bund_fence3 ~ com.dat.fence3$year + com.dat.fence3$area +    # no plot
                      com.dat.fence3$worm.count + 
                      com.dat.fence3$year:com.dat.fence3$area)

trym3.f7 <- manyglm(com_bund_fence3 ~ com.dat.fence3$year + com.dat.fence3$plot +   # no area
                      com.dat.fence3$worm.count + com.dat.fence3$year:com.dat.fence3$plot)

trym3.f8 <- manyglm(com_bund_fence3 ~ com.dat.fence3$area + com.dat.fence3$plot +   # no year
                      com.dat.fence3$worm.count + 
                      com.dat.fence3$area:com.dat.fence3$plot)

control.F3 <- how(within = Within(type = 'none'),
                  plots = Plots(strata = com.dat.fence3$plot_type, type = 'free'),
                  nperm = 1000)

permutations.F3 <- shuffleSet(nrow(com_bund_fence3), control = control.F3)

YPA.F3 <- anova(trym3.f0, trym3.f1, bootID = permutations.F3, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.F, "Native YPA Fenced.rds")
YP.F3 <- anova(trym3.f0, trym3.f2, bootID = permutations.F3, p.uni = 'adjusted', test = 'LR')
saveRDS(YP.F, "Native YP Fenced.rds")
YA.F3 <- anova(trym3.f0, trym3.f3, bootID = permutations.F3, p.uni = 'adjusted', test = 'LR')
saveRDS(YA.F, "Native YA Fenced.rds")
AP.F3 <- anova(trym3.f0, trym3.f4, bootID = permutations.F3, p.uni = 'adjusted', test = 'LR')
saveRDS(AP.F, "Native AP Fenced.rds")
worms.F3 <- anova(trym3.f0, trym3.f5, bootID = permutations.F3, p.uni = 'adjusted', test = 'LR')
saveRDS(worms.F, "Native Worms Fenced.rds")
plot.F3 <- anova(trym3.f0, trym3.f6, bootID = permutations.F3, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.F, "Native Plot Fenced.rds")
area.F3 <- anova(trym3.f0, trym3.f7, bootID = permutations.F3, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.F, "Native Area Fenced.rds")
year.F3 <- anova(trym3.f0, trym3.f8, bootID = permutations.F3, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.F, "Native Year Fenced.rds")


#m.f.final2 <- manyglm(com_bund_fence3 ~ )

### FITTING MODELS FOR INVASION 

control.I3 <- how(within = Within(type = 'none'),
                  plots = Plots(strata = com.dat.invade3$plot_type, type = 'free'),
                  nperm = 1000)

permutations.I3 <- shuffleSet(nrow(com_bund_invade3), control = control.I3)

trym3.i0 <- manyglm(com_bund_invade3 ~ com.dat.invade3$year + com.dat.invade3$area + com.dat.invade3$plot +   # complete model
                      com.dat.invade3$worm.count + 
                      com.dat.invade3$area:com.dat.invade3$plot + 
                      com.dat.invade3$year:com.dat.invade3$area + com.dat.invade3$year:com.dat.invade3$plot + 
                      com.dat.invade3$year:com.dat.invade3$area:com.dat.invade3$plot)

trym3.i1 <- manyglm(com_bund_invade3 ~ com.dat.invade3$year + com.dat.invade3$area + com.dat.invade3$plot +   # no 3way
                      com.dat.invade3$worm.count + 
                      com.dat.invade3$area:com.dat.invade3$plot + 
                      com.dat.invade3$year:com.dat.invade3$area + com.dat.invade3$year:com.dat.invade3$plot)

trym3.i2 <- manyglm(com_bund_invade3 ~ com.dat.invade3$year + com.dat.invade3$area + com.dat.invade3$plot +   # no year:plot
                      com.dat.invade3$worm.count + 
                      com.dat.invade3$area:com.dat.invade3$plot + 
                      com.dat.invade3$year:com.dat.invade3$area)

trym3.i3 <- manyglm(com_bund_invade3 ~ com.dat.invade3$year + com.dat.invade3$area + com.dat.invade3$plot +   # no year:area
                      com.dat.invade3$worm.count + 
                      com.dat.invade3$area:com.dat.invade3$plot + 
                      com.dat.invade3$year:com.dat.invade3$plot)

trym3.i4 <- manyglm(com_bund_invade3 ~ com.dat.invade3$year + com.dat.invade3$area + com.dat.invade3$plot +   # no ara:plot
                      com.dat.invade3$worm.count +
                      com.dat.invade3$year:com.dat.invade3$area + com.dat.invade3$year:com.dat.invade3$plot)

trym3.i5 <- manyglm(com_bund_invade3 ~ com.dat.invade3$year + com.dat.invade3$area + com.dat.invade3$plot +    # no worms
                      com.dat.invade3$area:com.dat.invade3$plot + 
                      com.dat.invade3$year:com.dat.invade3$area + com.dat.invade3$year:com.dat.invade3$plot + 
                      com.dat.invade3$year:com.dat.invade3$area:com.dat.invade3$plot)

trym3.i6 <- manyglm(com_bund_invade3 ~ com.dat.invade3$year + com.dat.invade3$area +    # no plot
                      com.dat.invade3$worm.count + 
                      com.dat.invade3$year:com.dat.invade3$area)

trym3.i7 <- manyglm(com_bund_invade3 ~ com.dat.invade3$year + com.dat.invade3$plot +   # no area
                      com.dat.invade3$worm.count + com.dat.invade3$year:com.dat.invade3$plot)

trym3.i8 <- manyglm(com_bund_invade3 ~ com.dat.invade3$area + com.dat.invade3$plot +   # no year
                      com.dat.invade3$worm.count + 
                      com.dat.invade3$area:com.dat.invade3$plot)

YPA.I3 <- anova(trym3.i0, trym3.i1, bootID = permutations.I3, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.I, "Native YPA Invaded.rds")
YP.I3 <- anova(trym3.i0, trym3.i2, bootID = permutations.I3, p.uni = 'adjusted', test = 'LR')
saveRDS(YP.I, "Native YP Invaded.rds")
YA.I3 <- anova(trym3.i0, trym3.i3, bootID = permutations.I3, p.uni = 'adjusted', test = 'LR')
saveRDS(YA.I, "Native YA Invaded.rds")
AP.I3 <- anova(trym3.i0, trym3.i4, bootID = permutations.I3, p.uni = 'adjusted', test = 'LR')
saveRDS(AP.I, "Native AP Invaded.rds")
worms.I3 <- anova(trym3.i0, trym3.i5, bootID = permutations.I3, p.uni = 'adjusted', test = 'LR')
saveRDS(worms.I, "Native Worms Invaded.rds")
plot.I3 <- anova(trym3.i0, trym3.i6, bootID = permutations.I3, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.I, "Native Plot Invaded.rds")
area.I3 <- anova(trym3.i0, trym3.i7, bootID = permutations.I3, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.I, "Native Area Invaded.rds")
year.I3 <- anova(trym3.i0, trym3.i8, bootID = permutations.I3, p.uni = 'adjusted', test = 'LR')
saveRDS(YPA.I, "Native Year Invaded.rds")

final.f3 <- manyglm(com_bund_fence3 ~ com.dat.fence3$year + com.dat.fence3$area + com.dat.fence3$plot +   # complete model
com.dat.fence3$worm.count +
  com.dat.fence3$year:com.dat.fence3$area)

final.i3 <- manyglm(com_bund_invade3 ~ com.dat.invade3$year + com.dat.invade3$area + com.dat.invade3$plot +   # complete model
com.dat.invade3$worm.count + 
  com.dat.invade3$area:com.dat.invade3$plot + 
  com.dat.invade3$year:com.dat.invade3$area + com.dat.invade3$year:com.dat.invade3$plot + 
  com.dat.invade3$year:com.dat.invade3$area:com.dat.invade3$plot)

final.output.F3 <- anova(final.f3, bootID = permutations.F3, p.uni = "adjusted")
final.output.I3 <- anova(final.i3, bootID = permutations.I3, p.uni = "adjusted")
saveRDS(final.output.F3, "MVABund Output Fencing Comparison Native")
saveRDS(final.output.I3, "MVABund Output Invasion Comparison Native")

## Exploring Deviance Values

#tests.F <- as.data.frame(final.output.F[["uni.test"]])
#rowSums(tests.F[which(colnames(tests.F) %in% forbspp)])/rowSums(tests.F)
#rowSums(tests.F[which(colnames(tests.F) %in% gramspp)])/rowSums(tests.F)
#rowSums(tests.F[which(colnames(tests.F) %in% woodyspp)])/rowSums(tests.F)

#x <- as.data.frame(c(mydata[["identifiers"]], mydata[["veg_calc"]], mydata[["worms"]]))

#ggplot(data = x, aes(x = year, y = sumwoody, fill = area)) + geom_bar(position = "dodge", stat = "summary", fun = "mean")
#ggplot(data = x, aes(x = year, y = sumforb, fill = area)) + geom_bar(position = "dodge", stat = "summary", fun = "mean")
#ggplot(data = x, aes(x = year, y = sumgram, fill = area)) + geom_bar(position = "dodge", stat = "summary", fun = "mean")

#ggplot(data = x, aes(x = ew.count.mean, y = sumwoody)) + geom_point() + geom_smooth()
#ggplot(data = x, aes(x = ew.count.mean, y = sumforb)) + geom_point() + geom_smooth()
#ggplot(data = x, aes(x = ew.count.mean, y = sumgram)) + geom_point() + geom_smooth()

#tests.I <- as.data.frame(final.output.I[["uni.test"]])
#rowSums(tests.I[which(colnames(tests.I) %in% forbspp)])/rowSums(tests.I)
#rowSums(tests.I[which(colnames(tests.I) %in% gramspp)])/rowSums(tests.I)
#rowSums(tests.I[which(colnames(tests.I) %in% woodyspp)])/rowSums(tests.I)














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