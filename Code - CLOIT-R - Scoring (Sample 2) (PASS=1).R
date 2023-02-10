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
cloit <- read.csv(file.choose())

describe(cloit)
names(cloit)

#***********************************************************
# Optional
# Compute means and standard deviations for each variable
var.Ms <- colMeans(cloit[,8:103], na.rm=T)
var.Ms
var.SD <- apply(cloit[,8:103], 2, sd, na.rm=T)
var.SD

# Compute means and standard deviations for each participant
ind.Ms <- rowMeans(cloit[,8:103], na.rm=T)
ind.Ms
ind.SD <- apply(cloit[,8:103], 1, sd, na.rm=T)
ind.SD
which(ind.SD < .25)
# Remove cases with standard deviations < .25
cloit2 <- cloit[-which(ind.SD < .25),]
# Remove cases where PASS = 1
cloit2 <- cloit[which(cloit$PASS==1),]
#***********************************************************

names(cloit2)
# Record "number of items checked" by summing across items (do before assigning new values)
NIC <- rowSums(cloit2[,8:103], na.rm=T)
cloit2$NIC <- rowSums(cloit2[,8:103], na.rm=T)

#***********************************************************
#Assign variables in columns 2, 4, and 6 (see below) with a value of 2 when the item is endorsed
cloit2$CLOIT17[cloit2$CLOIT17 == 1] = 2.
cloit2$CLOIT18[cloit2$CLOIT18 == 1] = 2.
cloit2$CLOIT19[cloit2$CLOIT19 == 1] = 2.
cloit2$CLOIT20[cloit2$CLOIT20 == 1] = 2.
cloit2$CLOIT21[cloit2$CLOIT21 == 1] = 2.
cloit2$CLOIT22[cloit2$CLOIT22 == 1] = 2.
cloit2$CLOIT23[cloit2$CLOIT23 == 1] = 2.
cloit2$CLOIT24[cloit2$CLOIT24 == 1] = 2.
cloit2$CLOIT25[cloit2$CLOIT25 == 1] = 2.
cloit2$CLOIT26[cloit2$CLOIT26 == 1] = 2.
cloit2$CLOIT27[cloit2$CLOIT27 == 1] = 2.
cloit2$CLOIT28[cloit2$CLOIT28 == 1] = 2.
cloit2$CLOIT29[cloit2$CLOIT29 == 1] = 2.
cloit2$CLOIT30[cloit2$CLOIT30 == 1] = 2.
cloit2$CLOIT31[cloit2$CLOIT31 == 1] = 2.
cloit2$CLOIT32[cloit2$CLOIT32 == 1] = 2.

cloit2$CLOIT49[cloit2$CLOIT49 == 1] = 2.
cloit2$CLOIT50[cloit2$CLOIT50 == 1] = 2.
cloit2$CLOIT51[cloit2$CLOIT51 == 1] = 2.
cloit2$CLOIT52[cloit2$CLOIT52 == 1] = 2.
cloit2$CLOIT53[cloit2$CLOIT53 == 1] = 2.
cloit2$CLOIT54[cloit2$CLOIT54 == 1] = 2.
cloit2$CLOIT55[cloit2$CLOIT55 == 1] = 2.
cloit2$CLOIT56[cloit2$CLOIT56 == 1] = 2.
cloit2$CLOIT57[cloit2$CLOIT57 == 1] = 2.
cloit2$CLOIT58[cloit2$CLOIT58 == 1] = 2.
cloit2$CLOIT59[cloit2$CLOIT59 == 1] = 2.
cloit2$CLOIT60[cloit2$CLOIT60 == 1] = 2.
cloit2$CLOIT61[cloit2$CLOIT61 == 1] = 2.
cloit2$CLOIT62[cloit2$CLOIT62 == 1] = 2.
cloit2$CLOIT63[cloit2$CLOIT63 == 1] = 2.
cloit2$CLOIT64[cloit2$CLOIT64 == 1] = 2.

cloit2$CLOIT81[cloit2$CLOIT81 == 1] = 2.
cloit2$CLOIT82[cloit2$CLOIT82 == 1] = 2.
cloit2$CLOIT83[cloit2$CLOIT83 == 1] = 2.
cloit2$CLOIT84[cloit2$CLOIT84 == 1] = 2.
cloit2$CLOIT85[cloit2$CLOIT85 == 1] = 2.
cloit2$CLOIT86[cloit2$CLOIT86 == 1] = 2.
cloit2$CLOIT87[cloit2$CLOIT87 == 1] = 2.
cloit2$CLOIT88[cloit2$CLOIT88 == 1] = 2.
cloit2$CLOIT89[cloit2$CLOIT89 == 1] = 2.
cloit2$CLOIT90[cloit2$CLOIT90 == 1] = 2.
cloit2$CLOIT91[cloit2$CLOIT91 == 1] = 2.
cloit2$CLOIT92[cloit2$CLOIT92 == 1] = 2.
cloit2$CLOIT93[cloit2$CLOIT93 == 1] = 2.
cloit2$CLOIT94[cloit2$CLOIT94 == 1] = 2.
cloit2$CLOIT95[cloit2$CLOIT95 == 1] = 2.
cloit2$CLOIT96[cloit2$CLOIT96 == 1] = 2.

# Record "average intensity" by average values (do after assigning new values)
cloit2$AIN <- rowSums(cloit2[,8:103], na.rm=T)/NIC

names(cloit2)
# Using scoreItems from psych package
?scoreItems
# Create a scoring key
cloit2.key <- list(CLOITA=c("CLOIT1","CLOIT17","CLOIT33","CLOIT49","CLOIT65","CLOIT81"),
                   CLOITD=c("CLOIT2","CLOIT18","CLOIT34","CLOIT50","CLOIT66","CLOIT82"),
                   CLOITG=c("CLOIT3","CLOIT19","CLOIT35","CLOIT51","CLOIT67","CLOIT83"),
                   CLOITJ=c("CLOIT4","CLOIT20","CLOIT36","CLOIT52","CLOIT68","CLOIT84"),
                   CLOITM=c("CLOIT5","CLOIT21","CLOIT37","CLOIT53","CLOIT69","CLOIT85"),
                   CLOITP=c("CLOIT6","CLOIT22","CLOIT38","CLOIT54","CLOIT70","CLOIT86"),
                   CLOITC=c("CLOIT7","CLOIT23","CLOIT39","CLOIT55","CLOIT71","CLOIT87"),
                   CLOITF=c("CLOIT8","CLOIT24","CLOIT40","CLOIT56","CLOIT72","CLOIT88"),
                   CLOITI=c("CLOIT9","CLOIT25","CLOIT41","CLOIT57","CLOIT73","CLOIT89"),
                   CLOITL=c("CLOIT10","CLOIT26","CLOIT42","CLOIT58","CLOIT74","CLOIT90"),
                   CLOITO=c("CLOIT11","CLOIT27","CLOIT43","CLOIT59","CLOIT75","CLOIT91"),
                   CLOITB=c("CLOIT12","CLOIT28","CLOIT44","CLOIT60","CLOIT76","CLOIT92"),
                   CLOITE=c("CLOIT13","CLOIT29","CLOIT45","CLOIT61","CLOIT77","CLOIT93"),
                   CLOITH=c("CLOIT14","CLOIT30","CLOIT46","CLOIT62","CLOIT78","CLOIT94"),
                   CLOITK=c("CLOIT15","CLOIT31","CLOIT47","CLOIT63","CLOIT79","CLOIT95"),
                   CLOITN=c("CLOIT16","CLOIT32","CLOIT48","CLOIT64","CLOIT80","CLOIT96")) 
# Score from key
scores <- scoreItems(cloit2.key, cloit2, min=0, max=2, totals=T) # To use sums instead of means, use totals=T (but be careful w/ missingness)
summary(scores) # Raw correlations below the diagonal, alpha on the diagonal, corrected correlations above the diagonal
describe(scores$scores) # Descriptive statistics (e.g., mean, standard deviation)
scoresOut <- data.frame("UserID"=cloit2$UserID, scores$scores)
scoresOut$CLOITTOT <- rowSums(scoresOut[,2:17], na.rm=T)
describe(scoresOut$CLOITTOT)
write.table(scoresOut, "CLOIT.csv", sep=",", col.names = NA)

corr1 <- corr.test(scoresOut[,2:17]) # Correlations among non-ipsatized scores
corr1

# Octants
scoresOut$CLOITPA <- scoresOut$CLOITP+scoresOut$CLOITA
scoresOut$CLOITBC <- scoresOut$CLOITB+scoresOut$CLOITC
scoresOut$CLOITDE <- scoresOut$CLOITD+scoresOut$CLOITE
scoresOut$CLOITFG <- scoresOut$CLOITF+scoresOut$CLOITG
scoresOut$CLOITHI <- scoresOut$CLOITH+scoresOut$CLOITI
scoresOut$CLOITJK <- scoresOut$CLOITJ+scoresOut$CLOITK
scoresOut$CLOITLM <- scoresOut$CLOITL+scoresOut$CLOITM
scoresOut$CLOITNO <- scoresOut$CLOITN+scoresOut$CLOITO

corr2 <- corr.test(scoresOut[,19:26]) # Correlations among non-ipsatized scores
corr2

scoresOut$CLOITAB <- scoresOut$CLOITA+scoresOut$CLOITB
scoresOut$CLOITCD <- scoresOut$CLOITC+scoresOut$CLOITD
scoresOut$CLOITEF <- scoresOut$CLOITE+scoresOut$CLOITF
scoresOut$CLOITGH <- scoresOut$CLOITG+scoresOut$CLOITH
scoresOut$CLOITIJ <- scoresOut$CLOITI+scoresOut$CLOITJ
scoresOut$CLOITKL <- scoresOut$CLOITK+scoresOut$CLOITL
scoresOut$CLOITMN <- scoresOut$CLOITM+scoresOut$CLOITN
scoresOut$CLOITOP <- scoresOut$CLOITO+scoresOut$CLOITP

corr3 <- corr.test(scoresOut[,27:34]) # Correlations among non-ipsatized scores
corr3

# Quadrants
scoresOut$CLOITHD <- (scoresOut$CLOITA*.707)+(scoresOut$CLOITB*.924)+(scoresOut$CLOITC)+(scoresOut$CLOITD*.924)+(scoresOut$CLOITE*.707)
scoresOut$CLOITHS <- (scoresOut$CLOITE*.707)+(scoresOut$CLOITF*.924)+(scoresOut$CLOITG)+(scoresOut$CLOITH*.924)+(scoresOut$CLOITI*.707)
scoresOut$CLOITFS <- (scoresOut$CLOITI*.707)+(scoresOut$CLOITJ*.924)+(scoresOut$CLOITK)+(scoresOut$CLOITL*.924)+(scoresOut$CLOITM*.707)
scoresOut$CLOITFD <- (scoresOut$CLOITM*.707)+(scoresOut$CLOITN*.924)+(scoresOut$CLOITO)+(scoresOut$CLOITP*.924)+(scoresOut$CLOITA*.707)

corr4 <- corr.test(scoresOut[,35:38]) # Correlations among non-ipsatized scores
corr4

# Hemispheres
scoresOut$CLOITDOM <- (scoresOut$CLOITN*.383)+(scoresOut$CLOITO*.707)+(scoresOut$CLOITP*.924)+(scoresOut$CLOITA)+(scoresOut$CLOITB*.924)+
  (scoresOut$CLOITC*.707)+(scoresOut$CLOITD*.383)
scoresOut$CLOITSUB <- (scoresOut$CLOITF*.383)+(scoresOut$CLOITG*.707)+(scoresOut$CLOITH*.924)+(scoresOut$CLOITI)+(scoresOut$CLOITJ*.924)+
  (scoresOut$CLOITK*.707)+(scoresOut$CLOITL*.383)
scoresOut$CLOITFRI <- (scoresOut$CLOITJ*.383)+(scoresOut$CLOITK*.707)+(scoresOut$CLOITL*.924)+(scoresOut$CLOITM)+(scoresOut$CLOITN*.924)+
  (scoresOut$CLOITO*.707)+(scoresOut$CLOITP*.383)
scoresOut$CLOITHOS <- (scoresOut$CLOITB*.383)+(scoresOut$CLOITC*.707)+(scoresOut$CLOITD*.924)+(scoresOut$CLOITE)+(scoresOut$CLOITF*.924)+
  (scoresOut$CLOITG*.707)+(scoresOut$CLOITH*.383)

corr5 <- corr.test(scoresOut[,39:42]) # Correlations among non-ipsatized scores
corr5

# Axes
scoresOut$CLOITCONTROL <- (scoresOut$CLOITA-scoresOut$CLOITI)+.924*(scoresOut$CLOITB+scoresOut$CLOITP-scoresOut$CLOITH-scoresOut$CLOITJ)+
  .707*(scoresOut$CLOITC+scoresOut$CLOITO-scoresOut$CLOITG-scoresOut$CLOITK)+.383*(scoresOut$CLOITD+scoresOut$CLOITN-scoresOut$CLOITF-scoresOut$CLOITL)
scoresOut$CLOITAFFILIA <- (scoresOut$CLOITM-scoresOut$CLOITE)+.924*(scoresOut$CLOITN+scoresOut$CLOITL-scoresOut$CLOITD-scoresOut$CLOITF)+
  .707*(scoresOut$CLOITO+scoresOut$CLOITK-scoresOut$CLOITC-scoresOut$CLOITG)+.383*(scoresOut$CLOITP+scoresOut$CLOITJ-scoresOut$CLOITB-scoresOut$CLOITH)

corr6 <- corr.test(scoresOut[,43:44]) # Correlations among non-ipsatized scores
corr6

# Merge CLOIT Scales (scoresOut) and NIC & AIN (cloit)
subVars <- with(cloit2, data.frame(UserID, NIC, AIN))
scoresOut <- merge(scoresOut, subVars, by="UserID")

#***************************************************************************
# Optional
# To get correlation among scales for each gender, use "tidyverse" or "dplyr"
# But will need to merge in Gender data for CLOIT
scoresOut2 <- data.frame("Gender"=cloit2$GENDER, scores$scores)
all <- scoresOut2 %>%
  split(.$Gender) %>%
  map(cor)

all[[2]][2:17, 2:17] # Toggle between "1" (Female) and "2" (Male)

# Similarly
varsKeep <- names(scoresOut)[c(2:17)]
some <- scoresOut2 %>%
  split(.$Gender) %>%
  map(select, varsKeep) %>%
  map(cor)
some[[2]]
#***************************************************************************

?t.test
t.test(CLOITPA~Gender, scoresOut, var.equal=F) # var.equal=T will return Student's t; var.equal=F will return Welch's t
t.test(CLOITBC~Gender, scoresOut, var.equal=F)
t.test(CLOITDE~Gender, scoresOut, var.equal=F)
t.test(CLOITFG~Gender, scoresOut, var.equal=F)
t.test(CLOITHI~Gender, scoresOut, var.equal=F)
t.test(CLOITJK~Gender, scoresOut, var.equal=F)
t.test(CLOITLM~Gender, scoresOut, var.equal=F)
t.test(CLOITNO~Gender, scoresOut, var.equal=F)
t.test(CLOITTOT~Gender, scoresOut, var.equal=F)
?describe.by
describeBy(scoresOut[,-1:-2], group=scoresOut$Gender)

# Correlations between Gender and CSIP scales, with p-values using Holm adjustment for multiple tests
corr.test(scoresOut$Gender, scoresOut[,-1:-2]) # Specifying $p after final parentheses will give exact p values

# PCA on non-ipsatized data
?principal
fit1 <- principal(scoresOut[,19:26], nfactors=2, rotate="varimax")
fit1

# Model Fit Tests of Circular Structure - Non-Ipsatized
?circ.tests
ct <- circ.tests(fit1)
ct

names(cloit2)
# Using scoreTest from multicon package
?scoreTest # If rel=F then you get just the scores; if rel=T then you get scores and reliability estimates
# Create a scoring key
cloit2.key <- list("CLOITA"=c(8,24,40,56,72,88),
                   "CLOITD"=c(9,25,41,57,73,89),
                   "CLOITG"=c(10,26,42,58,74,90),
                   "CLOITJ"=c(11,27,43,59,75,91),
                   "CLOITM"=c(12,28,44,60,76,92),
                   "CLOITP"=c(13,29,45,61,77,93),
                   "CLOITC"=c(14,30,46,62,78,94),
                   "CLOITF"=c(15,31,47,63,79,95),
                   "CLOITI"=c(16,32,48,64,80,96),
                   "CLOITL"=c(17,33,49,65,81,97),
                   "CLOITO"=c(18,34,50,66,82,98),
                   "CLOITB"=c(19,35,51,67,83,99),
                   "CLOITE"=c(20,36,52,68,84,100),
                   "CLOITH"=c(21,37,53,69,85,101),
                   "CLOITK"=c(22,38,54,70,86,102),
                   "CLOITN"=c(23,39,55,71,87,103))
# Score from key
scores <- scoreTest(cloit2, cloit2.key, nomiss=.80, minScore=0, maxScore=2) # nomiss indicates how much missingness is allowed
# To work with sums, run separate syntax files (scoreTest and sumif) then totals=T
summary(scores) 
describe(scores) # Descriptive statistics (e.g., mean, standard deviation)
scoresOut <- data.frame("UserID"=cloit$UserID, scores)
write.table(scores$scores, "cloit.csv", sep=",", col.names = NA)

corr1 <- corr.test(scoresOut[,2:17]) 
corr1 

# Octants
scoresOut$CLOITPA <- scoresOut$CLOITP+scoresOut$CLOITA
scoresOut$CLOITBC <- scoresOut$CLOITB+scoresOut$CLOITC
scoresOut$CLOITDE <- scoresOut$CLOITD+scoresOut$CLOITE
scoresOut$CLOITFG <- scoresOut$CLOITF+scoresOut$CLOITG
scoresOut$CLOITHI <- scoresOut$CLOITH+scoresOut$CLOITI
scoresOut$CLOITJK <- scoresOut$CLOITJ+scoresOut$CLOITK
scoresOut$CLOITLM <- scoresOut$CLOITL+scoresOut$CLOITM
scoresOut$CLOITNO <- scoresOut$CLOITN+scoresOut$CLOITO

scoresOut$CLOITAB <- scoresOut$CLOITA+scoresOut$CLOITB
scoresOut$CLOITCD <- scoresOut$CLOITC+scoresOut$CLOITD
scoresOut$CLOITEF <- scoresOut$CLOITE+scoresOut$CLOITF
scoresOut$CLOITGH <- scoresOut$CLOITG+scoresOut$CLOITH
scoresOut$CLOITIJ <- scoresOut$CLOITI+scoresOut$CLOITJ
scoresOut$CLOITKL <- scoresOut$CLOITK+scoresOut$CLOITL
scoresOut$CLOITMN <- scoresOut$CLOITM+scoresOut$CLOITN
scoresOut$CLOITOP <- scoresOut$CLOITO+scoresOut$CLOITP

corr2 <- corr.test(scoresOut[,27:34]) 
corr2

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
ind.desc <- describe(t(cloit2[,15:78]))
ind.desc

# Within-person standardize (ipsatize)
?ipsatize
zcloit <- ipsatize(cloit2[,8:103])
zcloit

# Check that each person has mean=0 and SD=1 across 64 items
describe(t(zcloit))

# Compute descriptive statistics for each variable in "zcsip"
describe(zcloit)
summary(zcloit)

# PCA on Ipsatized data - Item Level
?principal
fit <- principal(zcloit, nfactors=2, rotate="varimax")
fit
# Write the output to your working director as a .csv file with the name "mytable.csv"
write.table(fit$loadings, "mytable.csv", sep=",", col.names = NA)
# Alternatively, to get multiple elements, save as dataframe
fitOut <- data.frame(fit$loadings[], "r2"=fit$communality)
write.table(fitOut, "mytable.csv", sep=",", col.names=NA)

# Center CSIP Variables (No standardizing)
cloit.cent <- t(apply(cloit2[,8:103], 1, function(x) x - mean(x)))

# Using scoreItems from psych package to score ipsatized data
# Create a scoring key
cloit2.key <- list(CLOITA.ip=c("CLOIT1.ip","CLOIT17.ip","CLOIT33.ip","CLOIT49.ip","CLOIT65.ip","CLOIT81.ip"),
                   CLOITD.ip=c("CLOIT2.ip","CLOIT18.ip","CLOIT34.ip","CLOIT50.ip","CLOIT66.ip","CLOIT82.ip"),
                   CLOITG.ip=c("CLOIT3.ip","CLOIT19.ip","CLOIT35.ip","CLOIT51.ip","CLOIT67.ip","CLOIT83.ip"),
                   CLOITJ.ip=c("CLOIT4.ip","CLOIT20.ip","CLOIT36.ip","CLOIT52.ip","CLOIT68.ip","CLOIT84.ip"),
                   CLOITM.ip=c("CLOIT5.ip","CLOIT21.ip","CLOIT37.ip","CLOIT53.ip","CLOIT69.ip","CLOIT85.ip"),
                   CLOITP.ip=c("CLOIT6.ip","CLOIT22.ip","CLOIT38.ip","CLOIT54.ip","CLOIT70.ip","CLOIT86.ip"),
                   CLOITC.ip=c("CLOIT7.ip","CLOIT23.ip","CLOIT39.ip","CLOIT55.ip","CLOIT71.ip","CLOIT87.ip"),
                   CLOITF.ip=c("CLOIT8.ip","CLOIT24.ip","CLOIT40.ip","CLOIT56.ip","CLOIT72.ip","CLOIT88.ip"),
                   CLOITI.ip=c("CLOIT9.ip","CLOIT25.ip","CLOIT41.ip","CLOIT57.ip","CLOIT73.ip","CLOIT89.ip"),
                   CLOITL.ip=c("CLOIT10.ip","CLOIT26.ip","CLOIT42.ip","CLOIT58.ip","CLOIT74.ip","CLOIT90.ip"),
                   CLOITO.ip=c("CLOIT11.ip","CLOIT27.ip","CLOIT43.ip","CLOIT59.ip","CLOIT75.ip","CLOIT91.ip"),
                   CLOITB.ip=c("CLOIT12.ip","CLOIT28.ip","CLOIT44.ip","CLOIT60.ip","CLOIT76.ip","CLOIT92.ip"),
                   CLOITE.ip=c("CLOIT13.ip","CLOIT29.ip","CLOIT45.ip","CLOIT61.ip","CLOIT77.ip","CLOIT93.ip"),
                   CLOITH.ip=c("CLOIT14.ip","CLOIT30.ip","CLOIT46.ip","CLOIT62.ip","CLOIT78.ip","CLOIT94.ip"),
                   CLOITK.ip=c("CLOIT15.ip","CLOIT31.ip","CLOIT47.ip","CLOIT63.ip","CLOIT79.ip","CLOIT95.ip"),
                   CLOITN.ip=c("CLOIT16.ip","CLOIT32.ip","CLOIT48.ip","CLOIT64.ip","CLOIT80.ip","CLOIT96.ip"))
# Score from key
zscores <- scoreItems(cloit2.key, zcloit, min=0, max=3)
summary(zscores) # Raw correlations below the diagonal, alpha on the diagonal, corrected correlations above the diagonal
describe(zscores$scores) # Descriptive statistics (e.g., mean, standard deviation)
zscoresOut <- data.frame("UserID"=cloit2$UserID, zscores$scores)
write.table(zscoresOut, "ZCLOIT.csv", sep=",", col.names = NA)

zcorr <- corr.test(zscoresOut[,2:17]) # Correlations among ipsatized scores
zcorr

# Octants
zscoresOut$CLOITPA.ip <- zscoresOut$CLOITP.ip+zscoresOut$CLOITA.ip
zscoresOut$CLOITBC.ip <- zscoresOut$CLOITB.ip+zscoresOut$CLOITC.ip
zscoresOut$CLOITDE.ip <- zscoresOut$CLOITD.ip+zscoresOut$CLOITE.ip
zscoresOut$CLOITFG.ip <- zscoresOut$CLOITF.ip+zscoresOut$CLOITG.ip
zscoresOut$CLOITHI.ip <- zscoresOut$CLOITH.ip+zscoresOut$CLOITI.ip
zscoresOut$CLOITJK.ip <- zscoresOut$CLOITJ.ip+zscoresOut$CLOITK.ip
zscoresOut$CLOITLM.ip <- zscoresOut$CLOITL.ip+zscoresOut$CLOITM.ip
zscoresOut$CLOITNO.ip <- zscoresOut$CLOITN.ip+zscoresOut$CLOITO.ip

zcorr2 <- corr.test(zscoresOut[,18:25]) # Correlations among ipsatized scores
zcorr2

zscoresOut$CLOITAB.ip <- zscoresOut$CLOITA.ip+zscoresOut$CLOITB.ip
zscoresOut$CLOITCD.ip <- zscoresOut$CLOITC.ip+zscoresOut$CLOITD.ip
zscoresOut$CLOITEF.ip <- zscoresOut$CLOITE.ip+zscoresOut$CLOITF.ip
zscoresOut$CLOITGH.ip <- zscoresOut$CLOITG.ip+zscoresOut$CLOITH.ip
zscoresOut$CLOITIJ.ip <- zscoresOut$CLOITI.ip+zscoresOut$CLOITJ.ip
zscoresOut$CLOITKL.ip <- zscoresOut$CLOITK.ip+zscoresOut$CLOITL.ip
zscoresOut$CLOITMN.ip <- zscoresOut$CLOITM.ip+zscoresOut$CLOITN.ip
zscoresOut$CLOITOP.ip <- zscoresOut$CLOITO.ip+zscoresOut$CLOITP.ip

zcorr3 <- corr.test(zscoresOut[,26:33]) # Correlations among ipsatized scores
zcorr3

# Quadrants
zscoresOut$CLOITHD.ip <- (zscoresOut$CLOITA.ip*.707)+(zscoresOut$CLOITB.ip*.924)+(zscoresOut$CLOITC.ip)+(zscoresOut$CLOITD.ip*.924)+(zscoresOut$CLOITE.ip*.707)
zscoresOut$CLOITHS.ip <- (zscoresOut$CLOITE.ip*.707)+(zscoresOut$CLOITF.ip*.924)+(zscoresOut$CLOITG.ip)+(zscoresOut$CLOITH.ip*.924)+(zscoresOut$CLOITI.ip*.707)
zscoresOut$CLOITFS.ip <- (zscoresOut$CLOITI.ip*.707)+(zscoresOut$CLOITJ.ip*.924)+(zscoresOut$CLOITK.ip)+(zscoresOut$CLOITL.ip*.924)+(zscoresOut$CLOITM.ip*.707)
zscoresOut$CLOITFD.ip <- (zscoresOut$CLOITM.ip*.707)+(zscoresOut$CLOITN.ip*.924)+(zscoresOut$CLOITO.ip)+(zscoresOut$CLOITP.ip*.924)+(zscoresOut$CLOITA.ip*.707)

zcorr4 <- corr.test(zscoresOut[,34:37]) # Correlations among ipsatized scores
zcorr4

# Hemispheres
zscoresOut$CLOITDOM.ip <- (zscoresOut$CLOITN.ip*.383)+(zscoresOut$CLOITO.ip*.707)+(zscoresOut$CLOITP.ip*.924)+(zscoresOut$CLOITA.ip)+(zscoresOut$CLOITB.ip*.924)+
  (zscoresOut$CLOITC.ip*.707)+(zscoresOut$CLOITD.ip*.383)
zscoresOut$CLOITSUB.ip <- (zscoresOut$CLOITF.ip*.383)+(zscoresOut$CLOITG.ip*.707)+(zscoresOut$CLOITH.ip*.924)+(zscoresOut$CLOITI.ip)+(zscoresOut$CLOITJ.ip*.924)+
  (zscoresOut$CLOITK.ip*.707)+(zscoresOut$CLOITL.ip*.383)
zscoresOut$CLOITFRI.ip <- (zscoresOut$CLOITJ.ip*.383)+(zscoresOut$CLOITK.ip*.707)+(zscoresOut$CLOITL.ip*.924)+(zscoresOut$CLOITM.ip)+(zscoresOut$CLOITN.ip*.924)+
  (zscoresOut$CLOITO.ip*.707)+(zscoresOut$CLOITP.ip*.383)
zscoresOut$CLOITHOS.ip <- (zscoresOut$CLOITB.ip*.383)+(zscoresOut$CLOITC.ip*.707)+(zscoresOut$CLOITD.ip*.924)+(zscoresOut$CLOITE.ip)+(zscoresOut$CLOITF.ip*.924)+
  (zscoresOut$CLOITG.ip*.707)+(zscoresOut$CLOITH.ip*.383)

zcorr5 <- corr.test(zscoresOut[,38:41]) # Correlations among ipsatized scores
zcorr5

# Axes
zscoresOut$CLOITCONTROL.ip <- (zscoresOut$CLOITA.ip-zscoresOut$CLOITI.ip)+.924*(zscoresOut$CLOITB.ip+zscoresOut$CLOITP.ip-zscoresOut$CLOITH.ip-zscoresOut$CLOITJ.ip)+
  .707*(zscoresOut$CLOITC.ip+zscoresOut$CLOITO.ip-zscoresOut$CLOITG.ip-zscoresOut$CLOITK.ip)+.383*(zscoresOut$CLOITD.ip+zscoresOut$CLOITN.ip-zscoresOut$CLOITF.ip-zscoresOut$CLOITL.ip)
zscoresOut$CLOITAFFILIA.ip <- (zscoresOut$CLOITM.ip-zscoresOut$CLOITE.ip)+.924*(zscoresOut$CLOITN.ip+zscoresOut$CLOITL.ip-zscoresOut$CLOITD.ip-zscoresOut$CLOITF.ip)+
  .707*(zscoresOut$CLOITO.ip+zscoresOut$CLOITK.ip-zscoresOut$CLOITC.ip-zscoresOut$CLOITG.ip)+.383*(zscoresOut$CLOITP.ip+zscoresOut$CLOITJ.ip-zscoresOut$CLOITB.ip-zscoresOut$CLOITH.ip)

zcorr6 <- corr.test(zscoresOut[,42:43]) # Correlations among ipsatized scores
zcorr6

# PCA on Ipsatized data - Scale Level
?principal
fit2 <- principal(zscoresOut[,18:25], nfactors=2, rotate="varimax")
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

# Merge CLOIT (scoresOut) and ZCLOIT (zscoresOut)
total <- merge(scoresOut, zscoresOut, by="UserID")
write.table(total, "CLOIT_TOTAL.csv", sep=",", col.names = NA)

# Scale variables if participant answered 5/6 items per scale
cloit.out <- scoreTest(cloit.key, cloit.ip, rel=T, nomiss=5/6, totals=T)
describe(cloit.out$scores)
cloit.out$rel

# Rescaling for missingness
cloit.out <- scoreTest(cloit.key, cloit.ip, rel=T, nomiss = 5/6)
cloit.outX6 <- cloit.out$scores * 6
describe(cloit.outX6)