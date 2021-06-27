# Trying out a new way of analysing the com data with fencing and invasion variables

### FITTING MODELS

trym0 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$fencing +   # complete model
                  com.dat$invaded + com.dat$worm.count + 
                  com.dat$area:com.dat$fencing + com.dat$invaded:com.dat$fencing + 
                  com.dat$year:com.dat$area + com.dat$year:com.dat$fencing + 
                  com.dat$year:com.dat$invaded + com.dat$fencing:com.dat$worm.count +
                  com.dat$invaded:com.dat$worm.count + 
                  com.dat$year:com.dat$area:com.dat$fencing + 
                  com.dat$year:com.dat$invaded:com.dat$fencing + 
                  com.dat$invaded:com.dat$fencing:com.dat$worm.count)

trym1 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$fencing +   # no worms
                  com.dat$invaded + 
                  com.dat$area:com.dat$fencing + com.dat$invaded:com.dat$fencing + 
                  com.dat$year:com.dat$area + com.dat$year:com.dat$fencing + 
                  com.dat$year:com.dat$invaded +
                  com.dat$year:com.dat$area:com.dat$fencing + 
                  com.dat$year:com.dat$invaded:com.dat$fencing)

trym2 <- manyglm(com_bund ~ com.dat$area + com.dat$fencing +   # no year
                   com.dat$invaded + com.dat$worm.count + 
                   com.dat$area:com.dat$fencing + com.dat$invaded:com.dat$fencing + 
                   com.dat$fencing:com.dat$worm.count +
                   com.dat$invaded:com.dat$worm.count +
                   com.dat$invaded:com.dat$fencing:com.dat$worm.count)

trym3 <- manyglm(com_bund ~ com.dat$year + com.dat$fencing +   # no area
                   com.dat$invaded + com.dat$worm.count + 
                   com.dat$invaded:com.dat$fencing + 
                   com.dat$year:com.dat$fencing + 
                   com.dat$year:com.dat$invaded + com.dat$fencing:com.dat$worm.count +
                   com.dat$invaded:com.dat$worm.count + 
                   com.dat$year:com.dat$invaded:com.dat$fencing + 
                   com.dat$invaded:com.dat$fencing:com.dat$worm.count)

trym4 <- manyglm(com_bund ~ com.dat$year + com.dat$area +   # no fencing
                   com.dat$invaded + com.dat$worm.count + 
                   com.dat$year:com.dat$area + 
                   com.dat$year:com.dat$invaded + 
                   com.dat$invaded:com.dat$worm.count)

trym5 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$fencing +   # no invaded
                   com.dat$worm.count + 
                   com.dat$area:com.dat$fencing +  
                   com.dat$year:com.dat$area + com.dat$year:com.dat$fencing + 
                   com.dat$fencing:com.dat$worm.count +
                   com.dat$year:com.dat$area:com.dat$fencing)

trym6 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$fencing +   # no area:fencing
                   com.dat$invaded + com.dat$worm.count + 
                   com.dat$invaded:com.dat$fencing + 
                   com.dat$year:com.dat$area + com.dat$year:com.dat$fencing + 
                   com.dat$year:com.dat$invaded + com.dat$fencing:com.dat$worm.count +
                   com.dat$invaded:com.dat$worm.count +
                   com.dat$year:com.dat$invaded:com.dat$fencing + 
                   com.dat$invaded:com.dat$fencing:com.dat$worm.count)

trym7 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$fencing +   # no invaded:fencing
                   com.dat$invaded + com.dat$worm.count + 
                   com.dat$area:com.dat$fencing + 
                   com.dat$year:com.dat$area + com.dat$year:com.dat$fencing + 
                   com.dat$year:com.dat$invaded + com.dat$fencing:com.dat$worm.count +
                   com.dat$invaded:com.dat$worm.count + 
                   com.dat$year:com.dat$area:com.dat$fencing)

trym8 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$fencing +   # no year:area
                   com.dat$invaded + com.dat$worm.count + 
                   com.dat$area:com.dat$fencing + com.dat$invaded:com.dat$fencing + 
                   com.dat$year:com.dat$fencing + 
                   com.dat$year:com.dat$invaded + com.dat$fencing:com.dat$worm.count +
                   com.dat$invaded:com.dat$worm.count +
                   com.dat$year:com.dat$invaded:com.dat$fencing + 
                   com.dat$invaded:com.dat$fencing:com.dat$worm.count)

trym9 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$fencing +   # no year:fencing
                   com.dat$invaded + com.dat$worm.count + 
                   com.dat$area:com.dat$fencing + com.dat$invaded:com.dat$fencing + 
                   com.dat$year:com.dat$area +
                   com.dat$year:com.dat$invaded + com.dat$fencing:com.dat$worm.count +
                   com.dat$invaded:com.dat$worm.count +
                   com.dat$invaded:com.dat$fencing:com.dat$worm.count)

trym10 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$fencing +   # no year:invaded
                   com.dat$invaded + com.dat$worm.count + 
                   com.dat$area:com.dat$fencing + com.dat$invaded:com.dat$fencing + 
                   com.dat$year:com.dat$area + com.dat$year:com.dat$fencing + 
                   com.dat$fencing:com.dat$worm.count +
                   com.dat$invaded:com.dat$worm.count + 
                   com.dat$year:com.dat$area:com.dat$fencing +
                   com.dat$invaded:com.dat$fencing:com.dat$worm.count)

trym11 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$fencing +   # no fencing:worm
                   com.dat$invaded + com.dat$worm.count + 
                   com.dat$area:com.dat$fencing + com.dat$invaded:com.dat$fencing + 
                   com.dat$year:com.dat$area + com.dat$year:com.dat$fencing + 
                   com.dat$year:com.dat$invaded +
                   com.dat$invaded:com.dat$worm.count + 
                   com.dat$year:com.dat$area:com.dat$fencing + 
                   com.dat$year:com.dat$invaded:com.dat$fencing)

trym12 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$fencing +   # no invaded:worm
                   com.dat$invaded + com.dat$worm.count + 
                   com.dat$area:com.dat$fencing + com.dat$invaded:com.dat$fencing + 
                   com.dat$year:com.dat$area + com.dat$year:com.dat$fencing + 
                   com.dat$year:com.dat$invaded + com.dat$fencing:com.dat$worm.count +
                   com.dat$year:com.dat$area:com.dat$fencing + 
                   com.dat$year:com.dat$invaded:com.dat$fencing)

trym13 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$fencing +   # no year:area:fencing
                   com.dat$invaded + com.dat$worm.count + 
                   com.dat$area:com.dat$fencing + com.dat$invaded:com.dat$fencing + 
                   com.dat$year:com.dat$area + com.dat$year:com.dat$fencing + 
                   com.dat$year:com.dat$invaded + com.dat$fencing:com.dat$worm.count +
                   com.dat$invaded:com.dat$worm.count + 
                   com.dat$year:com.dat$invaded:com.dat$fencing + 
                   com.dat$invaded:com.dat$fencing:com.dat$worm.count)

trym14 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$fencing +   # no year:invaded:fencing
                   com.dat$invaded + com.dat$worm.count + 
                   com.dat$area:com.dat$fencing + com.dat$invaded:com.dat$fencing + 
                   com.dat$year:com.dat$area + com.dat$year:com.dat$fencing + 
                   com.dat$year:com.dat$invaded + com.dat$fencing:com.dat$worm.count +
                   com.dat$invaded:com.dat$worm.count + 
                   com.dat$year:com.dat$area:com.dat$fencing + 
                   com.dat$invaded:com.dat$fencing:com.dat$worm.count)

trym15 <- manyglm(com_bund ~ com.dat$year + com.dat$area + com.dat$fencing +   # no invaded:fencing:worms
                   com.dat$invaded + com.dat$worm.count + 
                   com.dat$area:com.dat$fencing + com.dat$invaded:com.dat$fencing + 
                   com.dat$year:com.dat$area + com.dat$year:com.dat$fencing + 
                   com.dat$year:com.dat$invaded + com.dat$fencing:com.dat$worm.count +
                   com.dat$invaded:com.dat$worm.count + 
                   com.dat$year:com.dat$area:com.dat$fencing + 
                   com.dat$year:com.dat$invaded:com.dat$fencing)

### MODEL COMPARISON

# to account for nested data

control <- how(within = Within(type = 'none'),
               plots = Plots(strata = com.dat$plot_type, type = 'free'),
               nperm = 1000)

permutations <- shuffleSet(nrow(com_bund), control = control)

worms. <- anova(trym0, trym1, bootID = permutations, p.uni = 'adjusted', test = 'LR')
year. <- anova(trym0, trym2, bootID = permutations, p.uni = 'adjusted', test = 'LR')
area. <- anova(trym0, trym3, bootID = permutations, p.uni = 'adjusted', test = 'LR')
fence. <- anova(trym0, trym4, bootID = permutations, p.uni = 'adjusted', test = 'LR')
invade. <- anova(trym0, trym5, bootID = permutations, p.uni = 'adjusted', test = 'LR')
area.fence. <- anova(trym0, trym6, bootID = permutations, p.uni = 'adjusted', test = 'LR')
invade.fence. <- anova(trym0, trym7, bootID = permutations, p.uni = 'adjusted', test = 'LR')
year.area. <- anova(trym0, trym8, bootID = permutations, p.uni = 'adjusted', test = 'LR')
year.fence. <- anova(trym0, trym9, bootID = permutations, p.uni = 'adjusted', test = 'LR')
year.invade. <- anova(trym0, trym10, bootID = permutations, p.uni = 'adjusted', test = 'LR')
fence.worms. <- anova(trym0, trym11, bootID = permutations, p.uni = 'adjusted', test = 'LR')
invade.worms. <- anova(trym0, trym12, bootID = permutations, p.uni = 'adjusted', test = 'LR')
year.area.fence. <- anova(trym0, trym13, bootID = permutations, p.uni = 'adjusted', test = 'LR')
year.invade.fence. <- anova(trym0, trym14, bootID = permutations, p.uni = 'adjusted', test = 'LR')
invade.fence.worms. <- anova(trym0, trym15, bootID = permutations, p.uni = 'adjusted', test = 'LR')





# non-invasives

### FITTING MODELS

non.inv.trym0 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$fencing +   # complete model
                   com.dat2$invaded + com.dat2$worm.count + 
                   com.dat2$area:com.dat2$fencing + com.dat2$invaded:com.dat2$fencing + 
                   com.dat2$year:com.dat2$area + com.dat2$year:com.dat2$fencing + 
                   com.dat2$year:com.dat2$invaded + com.dat2$fencing:com.dat2$worm.count +
                   com.dat2$invaded:com.dat2$worm.count + 
                   com.dat2$year:com.dat2$area:com.dat2$fencing + 
                   com.dat2$year:com.dat2$invaded:com.dat2$fencing + 
                   com.dat2$invaded:com.dat2$fencing:com.dat2$worm.count)

non.inv.trym1 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$fencing +   # no worms
                   com.dat2$invaded + 
                   com.dat2$area:com.dat2$fencing + com.dat2$invaded:com.dat2$fencing + 
                   com.dat2$year:com.dat2$area + com.dat2$year:com.dat2$fencing + 
                   com.dat2$year:com.dat2$invaded +
                   com.dat2$year:com.dat2$area:com.dat2$fencing + 
                   com.dat2$year:com.dat2$invaded:com.dat2$fencing)

non.inv.trym2 <- manyglm(com_bund2 ~ com.dat2$area + com.dat2$fencing +   # no year
                   com.dat2$invaded + com.dat2$worm.count + 
                   com.dat2$area:com.dat2$fencing + com.dat2$invaded:com.dat2$fencing + 
                   com.dat2$fencing:com.dat2$worm.count +
                   com.dat2$invaded:com.dat2$worm.count +
                   com.dat2$invaded:com.dat2$fencing:com.dat2$worm.count)

non.inv.trym3 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$fencing +   # no area
                   com.dat2$invaded + com.dat2$worm.count + 
                   com.dat2$invaded:com.dat2$fencing + 
                   com.dat2$year:com.dat2$fencing + 
                   com.dat2$year:com.dat2$invaded + com.dat2$fencing:com.dat2$worm.count +
                   com.dat2$invaded:com.dat2$worm.count + 
                   com.dat2$year:com.dat2$invaded:com.dat2$fencing + 
                   com.dat2$invaded:com.dat2$fencing:com.dat2$worm.count)

non.inv.trym4 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area +   # no fencing
                   com.dat2$invaded + com.dat2$worm.count + 
                   com.dat2$year:com.dat2$area + 
                   com.dat2$year:com.dat2$invaded + 
                   com.dat2$invaded:com.dat2$worm.count)

non.inv.trym5 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$fencing +   # no invaded
                   com.dat2$worm.count + 
                   com.dat2$area:com.dat2$fencing +  
                   com.dat2$year:com.dat2$area + com.dat2$year:com.dat2$fencing + 
                   com.dat2$fencing:com.dat2$worm.count +
                   com.dat2$year:com.dat2$area:com.dat2$fencing)

non.inv.trym6 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$fencing +   # no area:fencing
                   com.dat2$invaded + com.dat2$worm.count + 
                   com.dat2$invaded:com.dat2$fencing + 
                   com.dat2$year:com.dat2$area + com.dat2$year:com.dat2$fencing + 
                   com.dat2$year:com.dat2$invaded + com.dat2$fencing:com.dat2$worm.count +
                   com.dat2$invaded:com.dat2$worm.count +
                   com.dat2$year:com.dat2$invaded:com.dat2$fencing + 
                   com.dat2$invaded:com.dat2$fencing:com.dat2$worm.count)

non.inv.trym7 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$fencing +   # no invaded:fencing
                   com.dat2$invaded + com.dat2$worm.count + 
                   com.dat2$area:com.dat2$fencing + 
                   com.dat2$year:com.dat2$area + com.dat2$year:com.dat2$fencing + 
                   com.dat2$year:com.dat2$invaded + com.dat2$fencing:com.dat2$worm.count +
                   com.dat2$invaded:com.dat2$worm.count + 
                   com.dat2$year:com.dat2$area:com.dat2$fencing)

non.inv.trym8 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$fencing +   # no year:area
                   com.dat2$invaded + com.dat2$worm.count + 
                   com.dat2$area:com.dat2$fencing + com.dat2$invaded:com.dat2$fencing + 
                   com.dat2$year:com.dat2$fencing + 
                   com.dat2$year:com.dat2$invaded + com.dat2$fencing:com.dat2$worm.count +
                   com.dat2$invaded:com.dat2$worm.count +
                   com.dat2$year:com.dat2$invaded:com.dat2$fencing + 
                   com.dat2$invaded:com.dat2$fencing:com.dat2$worm.count)

non.inv.trym9 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$fencing +   # no year:fencing
                   com.dat2$invaded + com.dat2$worm.count + 
                   com.dat2$area:com.dat2$fencing + com.dat2$invaded:com.dat2$fencing + 
                   com.dat2$year:com.dat2$area +
                   com.dat2$year:com.dat2$invaded + com.dat2$fencing:com.dat2$worm.count +
                   com.dat2$invaded:com.dat2$worm.count +
                   com.dat2$invaded:com.dat2$fencing:com.dat2$worm.count)

non.inv.trym10 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$fencing +   # no year:invaded
                    com.dat2$invaded + com.dat2$worm.count + 
                    com.dat2$area:com.dat2$fencing + com.dat2$invaded:com.dat2$fencing + 
                    com.dat2$year:com.dat2$area + com.dat2$year:com.dat2$fencing + 
                    com.dat2$fencing:com.dat2$worm.count +
                    com.dat2$invaded:com.dat2$worm.count + 
                    com.dat2$year:com.dat2$area:com.dat2$fencing +
                    com.dat2$invaded:com.dat2$fencing:com.dat2$worm.count)

non.inv.trym11 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$fencing +   # no fencing:worm
                    com.dat2$invaded + com.dat2$worm.count + 
                    com.dat2$area:com.dat2$fencing + com.dat2$invaded:com.dat2$fencing + 
                    com.dat2$year:com.dat2$area + com.dat2$year:com.dat2$fencing + 
                    com.dat2$year:com.dat2$invaded +
                    com.dat2$invaded:com.dat2$worm.count + 
                    com.dat2$year:com.dat2$area:com.dat2$fencing + 
                    com.dat2$year:com.dat2$invaded:com.dat2$fencing)

non.inv.trym12 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$fencing +   # no invaded:worm
                    com.dat2$invaded + com.dat2$worm.count + 
                    com.dat2$area:com.dat2$fencing + com.dat2$invaded:com.dat2$fencing + 
                    com.dat2$year:com.dat2$area + com.dat2$year:com.dat2$fencing + 
                    com.dat2$year:com.dat2$invaded + com.dat2$fencing:com.dat2$worm.count +
                    com.dat2$year:com.dat2$area:com.dat2$fencing + 
                    com.dat2$year:com.dat2$invaded:com.dat2$fencing)

non.inv.trym13 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$fencing +   # no year:area:fencing
                    com.dat2$invaded + com.dat2$worm.count + 
                    com.dat2$area:com.dat2$fencing + com.dat2$invaded:com.dat2$fencing + 
                    com.dat2$year:com.dat2$area + com.dat2$year:com.dat2$fencing + 
                    com.dat2$year:com.dat2$invaded + com.dat2$fencing:com.dat2$worm.count +
                    com.dat2$invaded:com.dat2$worm.count + 
                    com.dat2$year:com.dat2$invaded:com.dat2$fencing + 
                    com.dat2$invaded:com.dat2$fencing:com.dat2$worm.count)

non.inv.trym14 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$fencing +   # no year:invaded:fencing
                    com.dat2$invaded + com.dat2$worm.count + 
                    com.dat2$area:com.dat2$fencing + com.dat2$invaded:com.dat2$fencing + 
                    com.dat2$year:com.dat2$area + com.dat2$year:com.dat2$fencing + 
                    com.dat2$year:com.dat2$invaded + com.dat2$fencing:com.dat2$worm.count +
                    com.dat2$invaded:com.dat2$worm.count + 
                    com.dat2$year:com.dat2$area:com.dat2$fencing + 
                    com.dat2$invaded:com.dat2$fencing:com.dat2$worm.count)

non.inv.trym15 <- manyglm(com_bund2 ~ com.dat2$year + com.dat2$area + com.dat2$fencing +   # no invaded:fencing:worms
                    com.dat2$invaded + com.dat2$worm.count + 
                    com.dat2$area:com.dat2$fencing + com.dat2$invaded:com.dat2$fencing + 
                    com.dat2$year:com.dat2$area + com.dat2$year:com.dat2$fencing + 
                    com.dat2$year:com.dat2$invaded + com.dat2$fencing:com.dat2$worm.count +
                    com.dat2$invaded:com.dat2$worm.count + 
                    com.dat2$year:com.dat2$area:com.dat2$fencing + 
                    com.dat2$year:com.dat2$invaded:com.dat2$fencing)

### MODEL COMPARISON

# to account for nested data

control2 <- how(within = Within(type = 'none'),
               plots = Plots(strata = com.dat2$plot_type, type = 'free'),
               nperm = 1000)

permutations2 <- shuffleSet(nrow(com_bund2), control = control2)

worms.2 <- anova(non.inv.trym0, non.inv.trym1, bootID = permutations2, p.uni = 'adjusted', test = 'LR')
year.2 <- anova(non.inv.trym0, non.inv.trym2, bootID = permutations2, p.uni = 'adjusted', test = 'LR')
area.2 <- anova(non.inv.trym0, non.inv.trym3, bootID = permutations2, p.uni = 'adjusted', test = 'LR')
fence.2 <- anova(non.inv.trym0, non.inv.trym4, bootID = permutations2, p.uni = 'adjusted', test = 'LR')
invade.2 <- anova(non.inv.trym0, non.inv.trym5, bootID = permutations2, p.uni = 'adjusted', test = 'LR')
area.fence.2 <- anova(non.inv.trym0, non.inv.trym6, bootID = permutations2, p.uni = 'adjusted', test = 'LR')
invade.fence.2 <- anova(non.inv.trym0, non.inv.trym7, bootID = permutations2, p.uni = 'adjusted', test = 'LR')
year.area.2 <- anova(non.inv.trym0, non.inv.trym8, bootID = permutations2, p.uni = 'adjusted', test = 'LR')
year.fence.2 <- anova(non.inv.trym0, non.inv.trym9, bootID = permutations2, p.uni = 'adjusted', test = 'LR')
year.invade.2 <- anova(non.inv.trym0, non.inv.trym10, bootID = permutations2, p.uni = 'adjusted', test = 'LR')
fence.worms.2 <- anova(non.inv.trym0, non.inv.trym11, bootID = permutations2, p.uni = 'adjusted', test = 'LR')
invade.worms.2 <- anova(non.inv.trym0, non.inv.trym12, bootID = permutations2, p.uni = 'adjusted', test = 'LR')
year.area.fence.2 <- anova(non.inv.trym0, non.inv.trym13, bootID = permutations2, p.uni = 'adjusted', test = 'LR')
year.invade.fence.2 <- anova(non.inv.trym0, non.inv.trym14, bootID = permutations2, p.uni = 'adjusted', test = 'LR')
invade.fence.worms.2 <- anova(non.inv.trym0, non.inv.trym15, bootID = permutations2, p.uni = 'adjusted', test = 'LR')




# natives

### FITTING MODELS

nat.trym0 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$fencing +   # complete model
                           com.dat3$invaded + com.dat3$worm.count + 
                           com.dat3$area:com.dat3$fencing + com.dat3$invaded:com.dat3$fencing + 
                           com.dat3$year:com.dat3$area + com.dat3$year:com.dat3$fencing + 
                           com.dat3$year:com.dat3$invaded + com.dat3$fencing:com.dat3$worm.count +
                           com.dat3$invaded:com.dat3$worm.count + 
                           com.dat3$year:com.dat3$area:com.dat3$fencing + 
                           com.dat3$year:com.dat3$invaded:com.dat3$fencing + 
                           com.dat3$invaded:com.dat3$fencing:com.dat3$worm.count)

nat.trym1 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$fencing +   # no worms
                           com.dat3$invaded + 
                           com.dat3$area:com.dat3$fencing + com.dat3$invaded:com.dat3$fencing + 
                           com.dat3$year:com.dat3$area + com.dat3$year:com.dat3$fencing + 
                           com.dat3$year:com.dat3$invaded +
                           com.dat3$year:com.dat3$area:com.dat3$fencing + 
                           com.dat3$year:com.dat3$invaded:com.dat3$fencing)

nat.trym2 <- manyglm(com_bund3 ~ com.dat3$area + com.dat3$fencing +   # no year
                           com.dat3$invaded + com.dat3$worm.count + 
                           com.dat3$area:com.dat3$fencing + com.dat3$invaded:com.dat3$fencing + 
                           com.dat3$fencing:com.dat3$worm.count +
                           com.dat3$invaded:com.dat3$worm.count +
                           com.dat3$invaded:com.dat3$fencing:com.dat3$worm.count)

nat.trym3 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$fencing +   # no area
                           com.dat3$invaded + com.dat3$worm.count + 
                           com.dat3$invaded:com.dat3$fencing + 
                           com.dat3$year:com.dat3$fencing + 
                           com.dat3$year:com.dat3$invaded + com.dat3$fencing:com.dat3$worm.count +
                           com.dat3$invaded:com.dat3$worm.count + 
                           com.dat3$year:com.dat3$invaded:com.dat3$fencing + 
                           com.dat3$invaded:com.dat3$fencing:com.dat3$worm.count)

nat.trym4 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area +   # no fencing
                           com.dat3$invaded + com.dat3$worm.count + 
                           com.dat3$year:com.dat3$area + 
                           com.dat3$year:com.dat3$invaded + 
                           com.dat3$invaded:com.dat3$worm.count)

nat.trym5 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$fencing +   # no invaded
                           com.dat3$worm.count + 
                           com.dat3$area:com.dat3$fencing +  
                           com.dat3$year:com.dat3$area + com.dat3$year:com.dat3$fencing + 
                           com.dat3$fencing:com.dat3$worm.count +
                           com.dat3$year:com.dat3$area:com.dat3$fencing)

nat.trym6 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$fencing +   # no area:fencing
                           com.dat3$invaded + com.dat3$worm.count + 
                           com.dat3$invaded:com.dat3$fencing + 
                           com.dat3$year:com.dat3$area + com.dat3$year:com.dat3$fencing + 
                           com.dat3$year:com.dat3$invaded + com.dat3$fencing:com.dat3$worm.count +
                           com.dat3$invaded:com.dat3$worm.count +
                           com.dat3$year:com.dat3$invaded:com.dat3$fencing + 
                           com.dat3$invaded:com.dat3$fencing:com.dat3$worm.count)

nat.trym7 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$fencing +   # no invaded:fencing
                           com.dat3$invaded + com.dat3$worm.count + 
                           com.dat3$area:com.dat3$fencing + 
                           com.dat3$year:com.dat3$area + com.dat3$year:com.dat3$fencing + 
                           com.dat3$year:com.dat3$invaded + com.dat3$fencing:com.dat3$worm.count +
                           com.dat3$invaded:com.dat3$worm.count + 
                           com.dat3$year:com.dat3$area:com.dat3$fencing)

nat.trym8 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$fencing +   # no year:area
                           com.dat3$invaded + com.dat3$worm.count + 
                           com.dat3$area:com.dat3$fencing + com.dat3$invaded:com.dat3$fencing + 
                           com.dat3$year:com.dat3$fencing + 
                           com.dat3$year:com.dat3$invaded + com.dat3$fencing:com.dat3$worm.count +
                           com.dat3$invaded:com.dat3$worm.count +
                           com.dat3$year:com.dat3$invaded:com.dat3$fencing + 
                           com.dat3$invaded:com.dat3$fencing:com.dat3$worm.count)

nat.trym9 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$fencing +   # no year:fencing
                           com.dat3$invaded + com.dat3$worm.count + 
                           com.dat3$area:com.dat3$fencing + com.dat3$invaded:com.dat3$fencing + 
                           com.dat3$year:com.dat3$area +
                           com.dat3$year:com.dat3$invaded + com.dat3$fencing:com.dat3$worm.count +
                           com.dat3$invaded:com.dat3$worm.count +
                           com.dat3$invaded:com.dat3$fencing:com.dat3$worm.count)

nat.trym10 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$fencing +   # no year:invaded
                            com.dat3$invaded + com.dat3$worm.count + 
                            com.dat3$area:com.dat3$fencing + com.dat3$invaded:com.dat3$fencing + 
                            com.dat3$year:com.dat3$area + com.dat3$year:com.dat3$fencing + 
                            com.dat3$fencing:com.dat3$worm.count +
                            com.dat3$invaded:com.dat3$worm.count + 
                            com.dat3$year:com.dat3$area:com.dat3$fencing +
                            com.dat3$invaded:com.dat3$fencing:com.dat3$worm.count)

nat.trym11 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$fencing +   # no fencing:worm
                            com.dat3$invaded + com.dat3$worm.count + 
                            com.dat3$area:com.dat3$fencing + com.dat3$invaded:com.dat3$fencing + 
                            com.dat3$year:com.dat3$area + com.dat3$year:com.dat3$fencing + 
                            com.dat3$year:com.dat3$invaded +
                            com.dat3$invaded:com.dat3$worm.count + 
                            com.dat3$year:com.dat3$area:com.dat3$fencing + 
                            com.dat3$year:com.dat3$invaded:com.dat3$fencing)

nat.trym12 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$fencing +   # no invaded:worm
                            com.dat3$invaded + com.dat3$worm.count + 
                            com.dat3$area:com.dat3$fencing + com.dat3$invaded:com.dat3$fencing + 
                            com.dat3$year:com.dat3$area + com.dat3$year:com.dat3$fencing + 
                            com.dat3$year:com.dat3$invaded + com.dat3$fencing:com.dat3$worm.count +
                            com.dat3$year:com.dat3$area:com.dat3$fencing + 
                            com.dat3$year:com.dat3$invaded:com.dat3$fencing)

nat.trym13 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$fencing +   # no year:area:fencing
                            com.dat3$invaded + com.dat3$worm.count + 
                            com.dat3$area:com.dat3$fencing + com.dat3$invaded:com.dat3$fencing + 
                            com.dat3$year:com.dat3$area + com.dat3$year:com.dat3$fencing + 
                            com.dat3$year:com.dat3$invaded + com.dat3$fencing:com.dat3$worm.count +
                            com.dat3$invaded:com.dat3$worm.count + 
                            com.dat3$year:com.dat3$invaded:com.dat3$fencing + 
                            com.dat3$invaded:com.dat3$fencing:com.dat3$worm.count)

nat.trym14 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$fencing +   # no year:invaded:fencing
                            com.dat3$invaded + com.dat3$worm.count + 
                            com.dat3$area:com.dat3$fencing + com.dat3$invaded:com.dat3$fencing + 
                            com.dat3$year:com.dat3$area + com.dat3$year:com.dat3$fencing + 
                            com.dat3$year:com.dat3$invaded + com.dat3$fencing:com.dat3$worm.count +
                            com.dat3$invaded:com.dat3$worm.count + 
                            com.dat3$year:com.dat3$area:com.dat3$fencing + 
                            com.dat3$invaded:com.dat3$fencing:com.dat3$worm.count)

nat.trym15 <- manyglm(com_bund3 ~ com.dat3$year + com.dat3$area + com.dat3$fencing +   # no invaded:fencing:worms
                            com.dat3$invaded + com.dat3$worm.count + 
                            com.dat3$area:com.dat3$fencing + com.dat3$invaded:com.dat3$fencing + 
                            com.dat3$year:com.dat3$area + com.dat3$year:com.dat3$fencing + 
                            com.dat3$year:com.dat3$invaded + com.dat3$fencing:com.dat3$worm.count +
                            com.dat3$invaded:com.dat3$worm.count + 
                            com.dat3$year:com.dat3$area:com.dat3$fencing + 
                            com.dat3$year:com.dat3$invaded:com.dat3$fencing)

### MODEL COMPARISON

# to account for nested data

control3 <- how(within = Within(type = 'none'),
                plots = Plots(strata = com.dat3$plot_type, type = 'free'),
                nperm = 1000)

permutations3 <- shuffleSet(nrow(com_bund3), control = control3)

worms.3 <- anova(nat.trym0, nat.trym1, bootID = permutations3, p.uni = 'adjusted', test = 'LR')
year.3 <- anova(nat.trym0, nat.trym2, bootID = permutations3, p.uni = 'adjusted', test = 'LR')
area.3 <- anova(nat.trym0, nat.trym3, bootID = permutations3, p.uni = 'adjusted', test = 'LR')
fence.3 <- anova(nat.trym0, nat.trym4, bootID = permutations3, p.uni = 'adjusted', test = 'LR')
invade.3 <- anova(nat.trym0, nat.trym5, bootID = permutations3, p.uni = 'adjusted', test = 'LR')
area.fence.3 <- anova(nat.trym0, nat.trym6, bootID = permutations3, p.uni = 'adjusted', test = 'LR')
invade.fence.3 <- anova(nat.trym0, nat.trym7, bootID = permutations3, p.uni = 'adjusted', test = 'LR')
year.area.3 <- anova(nat.trym0, nat.trym8, bootID = permutations3, p.uni = 'adjusted', test = 'LR')
year.fence.3 <- anova(nat.trym0, nat.trym9, bootID = permutations3, p.uni = 'adjusted', test = 'LR')
year.invade.3 <- anova(nat.trym0, nat.trym10, bootID = permutations3, p.uni = 'adjusted', test = 'LR')
fence.worms.3 <- anova(nat.trym0, nat.trym11, bootID = permutations3, p.uni = 'adjusted', test = 'LR')
invade.worms.3 <- anova(nat.trym0, nat.trym12, bootID = permutations3, p.uni = 'adjusted', test = 'LR')
year.area.fence.3 <- anova(nat.trym0, nat.trym13, bootID = permutations3, p.uni = 'adjusted', test = 'LR')
year.invade.fence.3 <- anova(nat.trym0, nat.trym14, bootID = permutations3, p.uni = 'adjusted', test = 'LR')
invade.fence.worms.3 <- anova(nat.trym0, nat.trym15, bootID = permutations3, p.uni = 'adjusted', test = 'LR')

