# Trying out a new way of analysing the com data with fencing and invasion variables

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


tests.F2 <- t(tests.F)
tests.F2 %>% 
  rename("(Intercept)" = "int", "com.dat.fence$year" = "year", "com.dat.fence$area" = "area", 
         "dom.dat.fence$plot" = "plot", "com.dat.fence$worm.count" = "worms", 
         "com.dat.fence$year:com.dat.fence$area" = "YA")
