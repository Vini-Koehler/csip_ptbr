install.packages("circumplex")
install.packages("dplyr")
install.packages("tidyverse")
library(circumplex)
library(psych)
library(multicon)
library(dplyr)
library(tidyverse)

?dplyr
browseVignettes(package = "dplyr")
?circumplex
browseVignettes(package = "circumplex")

setwd("C:/Users/mboudreaux/OneDrive - Hogan Assessments/Desktop")
csip <- read.csv(file.choose())

describe(csip)
names(csip)

#***********************************************************
# Optional
# Compute means and standard deviations for each variable
var.Ms <- colMeans(csip[,15:78], na.rm=T)
var.Ms
var.SD <- apply(csip[,15:78], 2, sd, na.rm=T)
var.SD

# Compute means and standard deviations for each participant
ind.Ms <- rowMeans(csip[,15:78], na.rm=T)
ind.Ms
ind.SD <- apply(csip[,15:78], 1, sd, na.rm=T)
ind.SD
which(ind.SD < .25)
# Remove cases with standard deviations < .25
csip2 <- csip[-which(ind.SD < .25),]
# Remove cases where PASS = 1
csip2 <- csip[which(csip$PASS==1),]
#***********************************************************

names(csip2)
# Using scoreItems from psych package
?scoreItems
# Create a scoring key
csip2.key <- list(CSIPPA=c("CSIP1","CSIP9","CSIP17","CSIP25","CSIP33","CSIP41","CSIP49","CSIP57"),
                  CSIPBC=c("CSIP2","CSIP10","CSIP18","CSIP26","CSIP34","CSIP42","CSIP50","CSIP58"),
                  CSIPDE=c("CSIP3","CSIP11","CSIP19","CSIP27","CSIP35","CSIP43","CSIP51","CSIP59"),
                  CSIPFG=c("CSIP4","CSIP12","CSIP20","CSIP28","CSIP36","CSIP44","CSIP52","CSIP60"),
                  CSIPHI=c("CSIP5","CSIP13","CSIP21","CSIP29","CSIP37","CSIP45","CSIP53","CSIP61"),
                  CSIPJK=c("CSIP6","CSIP14","CSIP22","CSIP30","CSIP38","CSIP46","CSIP54","CSIP62"),
                  CSIPLM=c("CSIP7","CSIP15","CSIP23","CSIP31","CSIP39","CSIP47","CSIP55","CSIP63"),
                  CSIPNO=c("CSIP8","CSIP16","CSIP24","CSIP32","CSIP40","CSIP48","CSIP56","CSIP64")) 
# Score from key
scores <- scoreItems(csip2.key, csip2, min=0, max=3, totals=T) # To use sums instead of means, use totals=T (but be careful w/ missingness)
summary(scores) # Raw correlations below the diagonal, alpha on the diagonal, corrected correlations above the diagonal
describe(scores$scores) # Descriptive statistics (e.g., mean, standard deviation)
scoresOut <- data.frame("UserID"=csip2$UserID, "Gender"=csip2$GENDER, scores$scores)
scoresOut$CSIPTOT <- rowSums(scoresOut[,3:10], na.rm=T)
describe(scoresOut$CSIPTOT)
write.table(scoresOut, "CSIP.csv", sep=",", col.names = NA)

corr1 <- corr.test(scoresOut[,3:10]) # Correlations among non-ipsatized scores
corr1

#***************************************************************************
# Optional
# To get correlation among scales for each gender, use "tidyverse" or "dplyr
scoresOut2 <- data.frame("Gender"=csip2$GENDER, scores$scores)
all <- scoresOut2 %>%
  split(.$Gender) %>%
  map(cor)

all[[1]][2:9, 2:9] # Toggle between "0" (Female) and "1" (Male)

# Similarly
varsKeep <- names(scoresOut)[c(2:10)]
some <- scoresOut2 %>%
  split(.$Gender) %>%
  map(select, varsKeep) %>%
  map(cor)
some[[2]]
#***************************************************************************

?t.test
t.test(CSIPPA~Gender, scoresOut, var.equal=F) # var.equal=T will return Student's t; var.equal=F will return Welch's t
t.test(CSIPBC~Gender, scoresOut, var.equal=F)
t.test(CSIPDE~Gender, scoresOut, var.equal=F)
t.test(CSIPFG~Gender, scoresOut, var.equal=F)
t.test(CSIPHI~Gender, scoresOut, var.equal=F)
t.test(CSIPJK~Gender, scoresOut, var.equal=F)
t.test(CSIPLM~Gender, scoresOut, var.equal=F)
t.test(CSIPNO~Gender, scoresOut, var.equal=F)
t.test(CSIPTOT~Gender, scoresOut, var.equal=F)
?describe.by
describeBy(scoresOut[,-1:-2], group=scoresOut$Gender)

# Correlations between Gender and CSIP scales, with p-values using Holm adjustment for multiple tests
corr.test(scoresOut$Gender, scoresOut[,-1:-2]) # Specifying $p after final parentheses will give exact p values

# PCA on non-ipsatized data
?principal
fit1 <- principal(scoresOut[,3:10], nfactors=2, rotate="varimax")
fit1

# Model Fit Tests of Circular Structure - Non-Ipsatized
?circ.tests
ct <- circ.tests(fit1)
ct

names(csip2)
# Using scoreTest from multicon package
?scoreTest # If rel=F then you get just the scores; if rel=T then you get scores and reliability estimates
# Create a scoring key
csip2.key <- list("CSIPPA"=c(15,23,31,39,47,55,63,71),
                  "CSIPBC"=c(16,24,32,40,48,56,64,72),
                  "CSIPDE"=c(17,25,33,41,49,57,65,73),
                  "CSIPFG"=c(18,26,34,42,50,58,66,74),
                  "CSIPHI"=c(19,27,35,43,51,59,67,75),
                  "CSIPJK"=c(20,28,36,44,52,60,68,76),
                  "CSIPLM"=c(21,29,37,45,53,61,69,77),
                  "CSIPNO"=c(22,30,38,46,54,62,70,78))
# Score from key
scores <- scoreTest(csip2, csip2.key, nomiss=.80, minScore=0, maxScore=3) # nomiss indicates how much missingness is allowed
# To work with sums, run separate syntax files (scoreTest and sumif) then totals=T
summary(scores) 
describe(scores) # Descriptive statistics (e.g., mean, standard deviation)
scoresOut <- data.frame("UserID"=csip2$UserID, "Gender"=csip$GENDER, scores)
write.table(scores$scores, "csip.csv", sep=",", col.names = NA)

corr1 <- corr.test(scoresOut[,3:10]) 
corr1 

# Summary Structural Method
?structSumIPC
structSumIPC(cor(scores, use="p")) # p = pairwise.complete.obs

# Graphing
plot(cos(xx$DEG * (pi / 180)), sin(xx$DEG * (pi / 180)), xlim=c(-1.5,1.5), ylim=c(-1.5,1.5), type="")
circX <- seq(-1,1, by=.01)
circY <- sqrt(1 - circX^2)
lines(c(circX,-circX), c(circY,-circY))
lines(c(0,0), c(-1,1))
lines(c(-1,1), c(0,0))
text(cos(xx$DEG * (pi / 180)), sin(xx$DEG * (pi / 180)), labels=colnames(scores), cex=.75, 
     srt=0)

# PCA on non-ipsatized data
?principal
scoresOut <- data.frame(scores$scores)
fit <- principal(scoresOut[,-1:-2], nfactors=2, rotate="varimax")
fit

# Compute descriptive statistics for each participant
ind.desc <- describe(t(csip[,15:78]))
ind.desc

# Within-person standardize (ipsatize)
?ipsatize
zcsip <- ipsatize(csip2[,15:78])
zcsip

# Check that each person has mean=0 and SD=1 across 64 items
describe(t(zcsip))

# Compute descriptive statistics for each variable in "zcsip"
describe(zcsip)
summary(zcsip)

# PCA on Ipsatized data - Item Level
?principal
fit <- principal(zcsip, nfactors=2, rotate="varimax")
fit
# Write the output to your working director as a .csv file with the name "mytable.csv"
write.table(fit$loadings, "mytable.csv", sep=",", col.names = NA)
# Alternatively, to get multiple elements, save as dataframe
fitOut <- data.frame(fit$loadings[], "r2"=fit$communality)
write.table(fitOut, "mytable.csv", sep=",", col.names=NA)

# Center CSIP Variables (No standardizing)
csip2.cent <- t(apply(csip[,15:78], 1, function(x) x - mean(x)))

# Using scoreItems from psych package to score ipsatized data
# Create a scoring key
csip2.key <- list(CSIPPA.ip=c("CSIP1.ip","CSIP9.ip","CSIP17.ip","CSIP25.ip","CSIP33.ip","CSIP41.ip","CSIP49.ip","CSIP57.ip"),
                  CSIPBC.ip=c("CSIP2.ip","CSIP10.ip","CSIP18.ip","CSIP26.ip","CSIP34.ip","CSIP42.ip","CSIP50.ip","CSIP58.ip"),
                  CSIPDE.ip=c("CSIP3.ip","CSIP11.ip","CSIP19.ip","CSIP27.ip","CSIP35.ip","CSIP43.ip","CSIP51.ip","CSIP59.ip"),
                  CSIPFG.ip=c("CSIP4.ip","CSIP12.ip","CSIP20.ip","CSIP28.ip","CSIP36.ip","CSIP44.ip","CSIP52.ip","CSIP60.ip"),
                  CSIPHI.ip=c("CSIP5.ip","CSIP13.ip","CSIP21.ip","CSIP29.ip","CSIP37.ip","CSIP45.ip","CSIP53.ip","CSIP61.ip"),
                  CSIPJK.ip=c("CSIP6.ip","CSIP14.ip","CSIP22.ip","CSIP30.ip","CSIP38.ip","CSIP46.ip","CSIP54.ip","CSIP62.ip"),
                  CSIPLM.ip=c("CSIP7.ip","CSIP15.ip","CSIP23.ip","CSIP31.ip","CSIP39.ip","CSIP47.ip","CSIP55.ip","CSIP63.ip"),
                  CSIPNO.ip=c("CSIP8.ip","CSIP16.ip","CSIP24.ip","CSIP32.ip","CSIP40.ip","CSIP48.ip","CSIP56.ip","CSIP64.ip")) 
# Score from key
zscores <- scoreItems(csip2.key, zcsip, min=0, max=3)
summary(zscores) # Raw correlations below the diagonal, alpha on the diagonal, corrected correlations above the diagonal
describe(zscores$scores) # Descriptive statistics (e.g., mean, standard deviation)
zscoresOut <- data.frame("UserID"=csip2$UserID, "Gender"=csip2$GENDER, zscores$scores)
write.table(zscoresOut, "ZCSIP.csv", sep=",", col.names = NA)

zcorr <- corr.test(zscoresOut[,3:10]) # Correlations among ipsatized scores
zcorr

# PCA on Ipsatized data - Scale Level
?principal
fit2 <- principal(zscoresOut[,3:10], nfactors=2, rotate="varimax")
fit2

# Model Fit Tests of Circular Structure - Ipsatized
ct <- circ.tests(fit2)
ct

# Procrustes Rotation
?Procrustes
example("Procrustes")
c1 <- c(0,-sqrt(.5),-1,-sqrt(.5),0,sqrt(.5),1,sqrt(.5))
c2 <- c(1,sqrt(.5),0,-sqrt(.5),-1,-sqrt(.5),0,sqrt(.5))
Targ <- as.matrix(data.frame(c1,c2))
Targ

L <- fit2$loadings
L
factor.congruence(L, Targ)

L2 <- Procrustes(L, Targ)
L2

example("factor.congruence")
factor.congruence(L2$loadings, Targ)

# Merge CSIP (scoresOut) and ZCSIP (zscoresOut)
total <- merge(scoresOut, zscoresOut, by="UserID")
write.table(total, "CSIP_TOTAL.csv", sep=",", col.names = NA)

# Scale variables if participant answered 6/8 items per scale
csip2.out <- scoreTest(csip2.key, csip2.ip, rel=T, nomiss=6/8, totals=T)
describe(csip2.out$scores)
csip2.out$rel

# Rescaling for missingness
csip2.out <- scoreTest(csip2.key, csip2.ip, rel=T, nomiss = 6/8)
csip2.outX8 <- csip2.out$scores * 8
describe(csip2.outX8)