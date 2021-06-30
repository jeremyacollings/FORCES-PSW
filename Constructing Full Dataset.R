########## PREPARING DATASET FOR ANALYSIS ##########

##### PACKAGES #####

library(tidyverse)
library(vegan)
library(readxl)

##### IMPORT DATA #####

veg <- read_xlsx("FORCES Veg_Reduced.xlsx")
veg[veg > 0 & veg <= 5] <- 2.5
veg[veg > 5 & veg <= 25] <- 15
veg[veg > 25 & veg <= 50] <- 37.5
veg[veg > 50 & veg <= 75] <- 62.5
veg[veg > 75 & veg <= 100] <- 87.5
veg$year <- c(rep(1, times = 162), rep(3, times = 162))
veg_noPSW <- veg[,-87]


##summarize worms
worms <- read_xlsx("FORCES Worm Data 2018.xlsx")
w1<-cbind.data.frame(
  aggregate(SumTotM ~site +area+plot, data=worms, median),
  aggregate(SumTotM ~site +area+plot, data=worms, mean)[,4],
  aggregate(SumTotC ~site +area+plot, data=worms, median)[,4],
  aggregate(SumTotC ~site +area+plot, data=worms, mean)[,4])
names(w1)<-c("site", "area", "plot", "ew.mass.median", "ew.mass.mean", "ew.count.median", "ew.count.mean" )

#summarize worms per plot - 5 samples per plot. 
#Decided to use mean biomass (between June and Aug) for all species
#we need to add a paragraph describing earthworms composition and abundance - a plot showing spp. in the results

#merge files
dat <- merge(veg, w1, by = c("site", "area", "plot"), all = TRUE)
  #check it worked
dim(dat)
dim(veg)
dat[1:10, c(1:4, 92:95)]
tapply(dat$ew.mass.mean, list(dat$plot, dat$area,dat$site), mean)
 #should match w1 - and it does

##### COVER CALCULATIONS ##### 

# I want to calculate the sum of veg cover by native/nonnative and by lifeform
# I also want to calculate the average veg cover by these factors
# In the analysis, I will likely have to max the sum cover to 100 (as is cover may be > 100)
# In the analysis, I will likely need to transform the average cover

# Calculating cover for native and nonnative #

natspp <- c("aceneg","acerub", "acesac" , "agealt", "toxrad", "aritri", "aranud", "asacan", 
          "maican", "mairac", "polbif", "erican", "eurdiv", "nabalb",  "solcae", "solfle", 
          "solsp", "symsp", "impsp", "causp", "carcar", "hacvir", "hydvir", "carcon", 
          "corsp", "carhit", "carrad", "drysp", "polacr", "ampbra", "germac", "gerrob", 
          "carysp", "colcan", "linben", "lycsp", "lirtul", "tilame", "frasp", "cirlut", 
          "onosen", "oxasp", "sancan", "pinstr", "actsp", "aneacu", "podpel", "ranrec", 
          "thadio", "galcir", "mitdip", "tiacor", "statri", "parqui", "lapcom", "amesp", 
          "cardip", "carysp", "ostvir", "plaocc", "quesp", "faggra")   

nis <- c("allpet", "vinros", "vinmin", "tusfar", "berthu", "myosyl", "lonsp", "celorb", 
       "epihel", "rhacat", "rosmul", "dacglo", "ligvul")

forbspp <- c("vinros", "vinmin", "aritri", "aranud", "asacan", "maican", "mairac", "polbif", 
             "agealt", "erican", "eurdiv", "nabalb", "solcae", "solfle",
             "symsp", "tarsp", "tusfar", "impsp", "causp", "hacvir", "hydvir", 
             "myosyl", "allpet", "carcon", "ampbra", "germac", "gerrob", "colcan", "lamsp", 
             "cirlut", "epihel", "oxasp", "sancan", "versp", "actsp", "aneacu", "podpel", 
             "ranrec", "thadio", "geusp", "galsp", "galcir", "mitdip", "tiacor", "viosp", 
             "cardip", "lapcom", "rubsp", "solsp")

gramspp <- c("carhit", "carrad", "caresp", "poasp", "dacglo", "carpan")
  
woodyspp <- c("vibsp", "toxrad", "berthu", "carcar", "lonsp", "celorb", "corsp", "carysp", 
              "linben", "lirtul", "tilame", "frasp", "pinstr", "rhacat", "prusp", "rosmul", 
              "aceneg", "acerub", "acesac", "statri", "ulmsp", "parqui", "vitsp", 
              "woody1", "woody2", "ostvir", "quesp", "plaocc", "ligvul", "faggra", "amesp")


# Constructing mini-datasets with cover and diversity data for both years#
#for just 2018 I added an extra loop to get the year

#check if PSW is on column 90
which(names(dat)=="vinros")

sumcover <-c()
for(i in 1:dim(dat)[[1]]){
  x <- dat[i, c(6:86, 88:91)] #select quadrat and columns for analysis
  sumnat <- sum(x[which(colnames(x) %in% natspp)])
  sumnis <- sum(x[which(colnames(x) %in% nis)])
  sumforb <- sum(x[which(colnames(x) %in% forbspp)])
  sumgram <- sum(x[which(colnames(x) %in% gramspp)])
  sumwoody <- sum(x[which(colnames(x) %in% woodyspp)])
  sumtotal <- sum(x)
  sumcover <- rbind.data.frame(
  sumcover, cbind.data.frame(year= sumtotal, sumnat, sumnis, sumforb, sumgram, sumwoody))
}

avecover <- c()
for(i in 1:dim(dat)[[1]]){
  x <- dat[i, c(6:86, 88:91)] #select quadrat and columns for analysis
  avenat <- rowMeans(x[which(colnames(x) %in% natspp)])
  avenis <- rowMeans(x[which(colnames(x) %in% nis)])
  aveforb <- rowMeans(x[which(colnames(x) %in% forbspp)])
  avegram <- rowMeans(x[which(colnames(x) %in% gramspp)])
  avewoody <- rowMeans(x[which(colnames(x) %in% woodyspp)])
  avetotal <- rowMeans(x)
  avecover <- rbind.data.frame(
    avecover, cbind.data.frame(avetotal, avenat, avenis, aveforb, avegram, avewoody))
}

diversity <- c()
for(i in 1:dim(dat)[[1]]){
  x <- dat[i, c(6:86, 88:91)] #select quadrat and columns for analysis
  spprich.nat <- specnumber(x[which(colnames(x) %in% natspp)])
  spprich.nis <- specnumber(x[which(colnames(x) %in% nis)])
  spprich.forb <- specnumber(x[which(colnames(x) %in% forbspp)])
  spprich.gram <- specnumber(x[which(colnames(x) %in% gramspp)])
  spprich.woody <- specnumber(x[which(colnames(x) %in% woodyspp)])
  spprich.total <- specnumber(x)
  shan.nat <- diversity((x[which(colnames(x) %in% natspp)]), index = "shannon")
  shan.nis <- diversity((x[which(colnames(x) %in% nis)]), index = "shannon")
  shan.forb <- diversity((x[which(colnames(x) %in% forbspp)]), index = "shannon")
  shan.gram <- diversity((x[which(colnames(x) %in% gramspp)]), index = "shannon")
  shan.woody <- diversity((x[which(colnames(x) %in% woodyspp)]), index = "shannon")
  shan.total <- diversity((x), index = "shannon")
  simp.nat <- diversity((x[which(colnames(x) %in% natspp)]), index = "simpson")
  simp.nis <- diversity((x[which(colnames(x) %in% nis)]), index = "simpson")
  simp.forb <- diversity((x[which(colnames(x) %in% forbspp)]), index = "simpson")
  simp.gram <- diversity((x[which(colnames(x) %in% gramspp)]), index = "simpson")
  simp.woody <- diversity((x[which(colnames(x) %in% woodyspp)]), index = "simpson")
  simp.total <- diversity((x), index = "simpson")
  piel.nat <- shan.nat/log(spprich.nat)
  piel.nis <- shan.nis/log(spprich.nis)
  piel.forb <- shan.forb/log(spprich.forb)
  piel.gram <- shan.gram/log(spprich.gram)
  piel.woody <- shan.woody/log(spprich.woody)
  piel.total <- shan.total/log(spprich.total)
  diversity <- rbind.data.frame(
    diversity, cbind.data.frame(spprich.total, shan.total, simp.total, piel.total,
                                spprich.nat, shan.nat, simp.nat, piel.nat,
                                spprich.nis, shan.nis, simp.nis, piel.nis,
                                spprich.forb, shan.forb, simp.forb, piel.forb,
                                spprich.gram, shan.gram, simp.gram, piel.gram,
                                spprich.woody, shan.woody, simp.woody, piel.woody))
}

#putting it all into a single dataframe

dat2 <- cbind.data.frame(dat[,1:5], dat[, 92:95], sumcover, avecover, diversity) #dat2 has veg calc and identifiers
veg_calc <-data.frame(sumcover, avecover, diversity)
# making list 

mydata <- list("veg" = veg[,6:91], "veg_noPSW" = veg[,c(6:86, 88:91)], 
            "vinros"=veg[,87], "veg_calc" = veg_calc,
             "worms" = dat[,92:95], "identifiers" = veg[1:5])
#I changed the name to mydata becuase data is a function in R and may cause problems

