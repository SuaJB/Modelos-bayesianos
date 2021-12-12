setwd("/Users/Sua/OneDrive/Tesis/Análisis/Experimento/")
library(lavaan)
library(tidyverse)
library(reshape2)
library(ggplot2)
library(ggridges)
library(viridis)
library(RColorBrewer)
library(wesanderson)
library(ggthemes)
library(ggjoy)
library(ggpubr)
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(brms)
library(shinystan)
library(magrittr)
library(dplyr)
library(purrr)
library(forcats)
library(tidyr)
library(modelr)
library(ggdist)
library(tidybayes)
library(cowplot)
library(rstan)
library(ggrepel)
library(RColorBrewer)
library(gganimate)
library(posterior)
library(lme4)

#Factor scores

#MOTIVACIÓN

#GROUP 1####

#G1d1  
encg1d1 <- read.csv("enc1.g1.csv", header=T, sep=";")
  
encg1d1[encg1d1=="1 (Totalmente en desacuerdo)"] <- 1
encg1d1[encg1d1=="5 (Totalmente de acuerdo)"] <- 5

m2a  <- ' MI.1 =~  MI3 + MI1 + MI2  + MI4 
          MEI.1=~ MEI1 + MEI2 + MEI3
          MEE.1 =~  MEE1 + MEE2 + 1*MEE3 
          AM.1 =~ A3 + MEE4  + A1'
  
scoresg1d1 <- cfa(m2a, data=encg1d1, ordered=TRUE, estimator="ULS") 
  
sco <- data.frame(lavPredict(scoresg1d1, method = "EBM"))
 
scostd <- data.frame(sco)  
 
factscores.g1d1 <- cbind(encg1d1$carne, scostd)  

colnames(factscores.g1d1) <- c("carne", "MI.1", "MEI.1", "MEE.1", "AM.1")
   
#g1d2
encg1d1 <- read.csv("enc2.g1.csv", header=T, sep=";")

encg1d1[encg1d1=="1 (Totalmente en desacuerdo)"] <- 1
encg1d1[encg1d1=="5 (Totalmente de acuerdo)"] <- 5

m2a  <- ' MI.1 =~  MI3 + MI1 + MI2  + MI4 
          MEI.1=~ MEI1 + MEI2 + MEI3
          MEE.1 =~  MEE1 + MEE2 + 1*MEE3 
          AM.1 =~ A3 + MEE4  + A1'

scoresg1d1 <- cfa(m2a, data=encg1d1, ordered=TRUE, estimator="ULS") 

sco <- data.frame(lavPredict(scoresg1d1, method = "EBM"))

scostd <- data.frame(sco)  

factscores.g1d2 <- cbind(encg1d1$carne, scostd)  

colnames(factscores.g1d2) <- c("carne", "MI.2", "MEI.2", "MEE.2", "AM.2")

#G1D3

encg1d1 <- read.csv("enc3.g1.csv", header=T, sep=";")

encg1d1[encg1d1=="1 (Totalmente en desacuerdo)"] <- 1
encg1d1[encg1d1=="5 (Totalmente de acuerdo)"] <- 5

m2a  <- ' MI.1 =~  MI3 + MI1 + MI2  + MI4 
          MEI.1=~ MEI1 + MEI2 + MEI3
          MEE.1 =~  MEE1 + MEE2 + 1*MEE3 
          AM.1 =~ A3 + MEE4  + A1'

scoresg1d1 <- cfa(m2a, data=encg1d1, ordered=TRUE, estimator="ULS") 

sco <- data.frame(lavPredict(scoresg1d1, method = "EBM"))

scostd <- data.frame(sco)  

factscores.g1d3 <- cbind(encg1d1$carne, scostd)  

colnames(factscores.g1d3) <- c("carne", "MI.3", "MEI.3", "MEE.3", "AM.3")



factscores.g1 <- merge(factscores.g1d1, factscores.g1d2, by="carne")

factscores.g1 <- merge(factscores.g1, factscores.g1d3, by="carne")

factscores.g1 <- factscores.g1[!duplicated(factscores.g1$carne),]

#GROUP 2####

#g2d1  
encg2d1 <- read.csv("enc1.g2.csv", header=T, sep=";")

encg2d1[encg2d1=="1 (Totalmente en desacuerdo)"] <- 1
encg2d1[encg2d1=="5 (Totalmente de acuerdo)"] <- 5

m2a  <- ' MI.1 =~  MI3 + MI1 + MI2  + MI4 
          MEI.1=~ MEI1 + MEI2 + MEI3
          MEE.1 =~  MEE1 + MEE2 + 1*MEE3 
          AM.1 =~ A3 + MEE4  + A1'

scoresg2d1 <- cfa(m2a, data=encg2d1, ordered=TRUE, estimator="ULS") 

sco <- data.frame(lavPredict(scoresg2d1, method = "EBM"))

scostd <- data.frame(sco)  

factscores.g2d1 <- cbind(encg2d1$Carne, scostd)  
colnames(factscores.g2d1) <- c("carne", "MI.1", "MEI.1", "MEE.1", "AM.1")

#g2d2
encg2d1 <- read.csv("enc2.g2.csv", header=T, sep=";")

encg2d1[encg2d1=="1 (Totalmente en desacuerdo)"] <- 1
encg2d1[encg2d1=="5 (Totalmente de acuerdo)"] <- 5

m2a  <- ' MI.1 =~  MI3 + MI1 + MI2  + MI4 
          MEI.1=~ MEI1 + MEI2 + MEI3
          MEE.1 =~  MEE1 + MEE2 + 1*MEE3 
          AM.1 =~ A3 + MEE4  + A1'

scoresg2d1 <- cfa(m2a, data=encg2d1, ordered=TRUE, estimator="ULS") 

sco <- data.frame(lavPredict(scoresg2d1, method = "EBM"))

scostd <- data.frame(sco)  

factscores.g2d2 <- cbind(encg2d1$Carne, scostd)  

colnames(factscores.g2d2) <- c("carne", "MI.2", "MEI.2", "MEE.2", "AM.2")

#g2D3

encg2d1 <- read.csv("enc3.g2.csv", header=T, sep=";")

encg2d1[encg2d1=="1 (Totalmente en desacuerdo)"] <- 1
encg2d1[encg2d1=="5 (Totalmente de acuerdo)"] <- 5

m2a  <- ' MI.1 =~  MI3 + MI1 + MI2  + MI4 
          MEI.1=~ MEI1 + MEI2 + MEI3
          MEE.1 =~  MEE1 + MEE2 + 1*MEE3 
          AM.1 =~ A3 + MEE4  + A1'

scoresg2d1 <- cfa(m2a, data=encg2d1, ordered=TRUE, estimator="ULS") 

sco <- data.frame(lavPredict(scoresg2d1, method = "EBM"))

scostd <- data.frame(sco)  

factscores.g2d3 <- cbind(encg2d1$Carne, scostd)  

colnames(factscores.g2d3) <- c("carne", "MI.3", "MEI.3", "MEE.3", "AM.3")




factscores.g2 <- merge(factscores.g2d1, factscores.g2d2, by="carne")

factscores.g2 <- merge(factscores.g2, factscores.g2d3, by="carne")

factscores.g2 <- factscores.g2[!duplicated(factscores.g2$carne),]

#GROUP 6####

#g6d1  
encg6d1 <- read.csv("enc1.g6.csv", header=T, sep=";")

encg6d1[encg6d1=="1 (Totalmente en desacuerdo)"] <- 1
encg6d1[encg6d1=="5 (Totalmente de acuerdo)"] <- 5

m2a  <- ' MI.1 =~  MI3 + MI1 + MI2  + MI4 
          MEI.1=~ MEI1 + MEI2 + MEI3
          MEE.1 =~  MEE1 + MEE2 + 1*MEE3 
          AM.1 =~ A3 + MEE4  + A1'

scoresg6d1 <- cfa(m2a, data=encg6d1, ordered=TRUE, estimator="ULS") 

sco <- data.frame(lavPredict(scoresg6d1, method = "EBM"))

scostd <- data.frame(sco)  

factscores.g6d1 <- cbind(encg6d1$Carne, scostd)  
colnames(factscores.g6d1) <- c("carne", "MI.1", "MEI.1", "MEE.1", "AM.1")

#g6d2
encg6d1 <- read.csv("enc2.g6.csv", header=T, sep=";")

encg6d1[encg6d1=="1 (Totalmente en desacuerdo)"] <- 1
encg6d1[encg6d1=="5 (Totalmente de acuerdo)"] <- 5

m2a  <- ' MI.1 =~  MI3 + MI1 + MI2  + MI4 
          MEI.1=~ MEI1 + MEI2 + MEI3
          MEE.1 =~  MEE1 + MEE2 + 1*MEE3 
          AM.1 =~ A3 + MEE4  + A1'

scoresg6d1 <- cfa(m2a, data=encg6d1, ordered=TRUE, estimator="ULS") 

sco <- data.frame(lavPredict(scoresg6d1, method = "EBM"))

scostd <- data.frame(sco)  

factscores.g6d2 <- cbind(encg6d1$Carne, scostd)  

colnames(factscores.g6d2) <- c("carne", "MI.2", "MEI.2", "MEE.2", "AM.2")

#g6D3

encg6d1 <- read.csv("enc3.g6.csv", header=T, sep=";")

encg6d1[encg6d1=="1 (Totalmente en desacuerdo)"] <- 1
encg6d1[encg6d1=="5 (Totalmente de acuerdo)"] <- 5

m2a  <- ' MI.1 =~  MI3 + MI1 + MI2  + MI4 
          MEI.1=~ MEI1 + MEI2 + MEI3
          MEE.1 =~  MEE1 + MEE2 + 1*MEE3 
          AM.1 =~ A3 + MEE4  + A1'

scoresg6d1 <- cfa(m2a, data=encg6d1, ordered=TRUE, estimator="ULS") 

sco <- data.frame(lavPredict(scoresg6d1, method = "EBM"))

scostd <- data.frame(sco)  

factscores.g6d3 <- cbind(encg6d1$Carne, scostd)  

colnames(factscores.g6d3) <- c("carne", "MI.3", "MEI.3", "MEE.3", "AM.3")





factscores.g6 <- merge(factscores.g6d1, factscores.g6d2, by="carne")

factscores.g6 <- merge(factscores.g6, factscores.g6d3, by="carne")

factscores.g6 <- factscores.g6[!duplicated(factscores.g6$carne),]

#GROUP 7####

  #g7d1  
  encg7d1 <- read.csv("enc1.g7.csv", header=T, sep=";")

  encg7d1[encg7d1=="1 (Totalmente en desacuerdo)"] <- 1
  encg7d1[encg7d1=="5 (Totalmente de acuerdo)"] <- 5

  m2a  <- ' MI.1 =~  MI3 + MI1 + MI2  + MI4 
          MEI.1=~ MEI1 + MEI2 + MEI3
          MEE.1 =~  MEE1 + MEE2 + 1*MEE3 
          AM.1 =~ A3 + MEE4  + A1'
  
  scoresg7d1 <- cfa(m2a, data=encg7d1, ordered=TRUE, estimator="ULS") 
  
  sco <- data.frame(lavPredict(scoresg7d1, method = "EBM"))
  scostd <- data.frame(sco)  
  factscores.g7d1 <- cbind(encg7d1$Carne, scostd)  
  colnames(factscores.g7d1) <- c("carne", "MI.1", "MEI.1", "MEE.1", "AM.1")
  
  #g7d2
  encg7d1 <- read.csv("enc2.g7.csv", header=T, sep=";")
  
  encg7d1[encg7d1=="1 (Totalmente en desacuerdo)"] <- 1
  encg7d1[encg7d1=="5 (Totalmente de acuerdo)"] <- 5
  
  m2a  <- ' MI.1 =~  MI3 + MI1 + MI2  + MI4 
          MEI.1=~ MEI1 + MEI2 + MEI3
          MEE.1 =~  MEE1 + MEE2 + 1*MEE3 
          AM.1 =~ A3 + MEE4  + A1'
  
  scoresg7d1 <- cfa(m2a, data=encg7d1, ordered=TRUE, estimator="ULS") 
  summary(scoresg7d1, std=T)
  
  lavInspect(scoresg7d1, "cov.lv")
  
  sco <- data.frame(lavPredict(scoresg7d1, method = "EBM"))
  
  scostd <- data.frame(sco)  
  
  factscores.g7d2 <- cbind(encg7d1$Carne, scostd)  
  
  colnames(factscores.g7d2) <- c("carne", "MI.2", "MEI.2", "MEE.2", "AM.2")
  
  #g7D3
  
  encg7d1 <- read.csv("enc3.g7.csv", header=T, sep=";")
  
  encg7d1[encg7d1=="1 (Totalmente en desacuerdo)"] <- 1
  encg7d1[encg7d1=="5 (Totalmente de acuerdo)"] <- 5
  
  m2a  <- ' MI.1 =~  MI3 + MI1 + MI2  + MI4 
          MEI.1=~ MEI1 + MEI2 + MEI3
          MEE.1 =~  MEE1 + MEE2 + 1*MEE3 
          AM.1 =~ A3 + MEE4  + A1'
  
  scoresg7d1 <- cfa(m2a, data=encg7d1, ordered=TRUE, estimator="ULS") 
  
summary(scoresg7d1, std=T) ##The correlation between MI and MEI is above 0.9 but 
                          ##the rest of correlations and parameters are ok so it seems 
                          ## to be an outlier  

lavInspect(scoresg7d1, "cov.lv")

  
sco <- data.frame(lavPredict(scoresg7d1, method = "EBM"))
  
scostd <- data.frame(sco)/(10^(14))

factscores.g7d3 <- cbind(encg7d1$Carne, scostd)  
  
colnames(factscores.g7d3) <- c("carne", "MI.3", "MEI.3", "MEE.3", "AM.3")
  
  
  
  
  
factscores.g7 <- merge(factscores.g7d1, factscores.g7d2, by="carne")
  
factscores.g7 <- merge(factscores.g7, factscores.g7d3, by="carne")
  

factscores.g7 <- factscores.g7[!duplicated(factscores.g7$carne),]

####ALL GROUPS ####

group <- c(rep(1,73),rep(2, 47), rep(6, 71), rep(7,39))

final.scores.m <- data.frame(group, rbind(factscores.g1, factscores.g2, factscores.g6, factscores.g7))
final.scores.m  <- final.scores.m [!duplicated(final.scores.m $carne),]

G1 <- c(123,103,103,73)/123*100
G2 <- c(119, 92, 63, 47)/119*100
G6 <- c(124, 112, 95, 71)/124*100
G7 <- c(91, 81, 53, 39)/91*100

quest <- rbind(G1, G2, G6, G7)

colnames(quest) <- c("Qt1", "Qt2", "Qt3", "All")



#AUTOEVALUACIÓN####

#GROUP 1####

  #G1d1  
  encg1d1 <- read.csv("enc1.g1.csv", header=T, sep=";")
  
  encg1d1[encg1d1=="1 (Totalmente en desacuerdo)"] <- 1
  encg1d1[encg1d1=="5 (Totalmente de acuerdo)"] <- 5
  
  m2a  <- ' SA  =~  AU12 + AU13 + AU11 + AU3  + AU10 + AU4
          CO =~   AU7 + AU8
          OE =~  AU6 + AU1 + AU2 '
  
  scoresg1d1 <- cfa(m2a, data=encg1d1, ordered=TRUE, estimator="ULS") 
  
  sco <- data.frame(lavPredict(scoresg1d1, method = "EBM"))
  
  scostd <- data.frame(sco)  
  
  factscores.g1d1 <- cbind(encg1d1$carne, scostd)  
  colnames(factscores.g1d1) <- c("carne", "SA.1", "CO.1", "OE.1")
  
  #g1d2
  encg1d1 <- read.csv("enc2.g1.csv", header=T, sep=";")
  
  encg1d1[encg1d1=="1 (Totalmente en desacuerdo)"] <- 1
  encg1d1[encg1d1=="5 (Totalmente de acuerdo)"] <- 5
  
  m2a  <- ' SA  =~  AU12 + AU13 + AU11 + AU3  + AU10 + AU4
          CO =~   AU7 + AU8
          OE =~  AU6 + AU1 + AU2 '
  
  scoresg1d1 <- cfa(m2a, data=encg1d1, ordered=TRUE, estimator="ULS") 
  
  sco <- data.frame(lavPredict(scoresg1d1, method = "EBM"))
  
  scostd <- data.frame(sco)  
  
  factscores.g1d2 <- cbind(encg1d1$carne, scostd)  
  
  colnames(factscores.g1d2) <- c("carne", "SA.2", "CO.2", "OE.2")
  
  #G1D3
  
  encg1d1 <- read.csv("enc3.g1.csv", header=T, sep=";")
  
  encg1d1[encg1d1=="1 (Totalmente en desacuerdo)"] <- 1
  encg1d1[encg1d1=="5 (Totalmente de acuerdo)"] <- 5
  
  m2a  <- ' SA  =~  AU12 + AU13 + AU11 + AU3  + AU10 + AU4
          CO =~   AU7 + AU8
          OE =~  AU6 + AU1 + AU2 '
  
  scoresg1d1 <- cfa(m2a, data=encg1d1, ordered=TRUE, estimator="ULS") 
  
  sco <- data.frame(lavPredict(scoresg1d1, method = "EBM"))
  
  scostd <- data.frame(sco)  
  
  factscores.g1d3 <- cbind(encg1d1$carne, scostd)  
  
  colnames(factscores.g1d3) <- c("carne", "SA.3", "CO.3", "OE.3")
  
  
  
  factscores.g1 <- merge(factscores.g1d1, factscores.g1d2, by="carne")
  
  factscores.g1 <- merge(factscores.g1, factscores.g1d3, by="carne")
  
  factscores.g1 <- factscores.g1[!duplicated(factscores.g1$carne),]

#GROUP 2####

  #g2d1  
  encg2d1 <- read.csv("enc1.g2.csv", header=T, sep=";")
  
  encg2d1[encg2d1=="1 (Totalmente en desacuerdo)"] <- 1
  encg2d1[encg2d1=="5 (Totalmente de acuerdo)"] <- 5
  
  m2a  <- ' SA  =~  AU12 + AU13 + AU11 + AU3  + AU10 + AU4
          CO =~   AU7 + AU8
          OE =~  AU6 + AU1 + AU2 '
  
  scoresg2d1 <- cfa(m2a, data=encg2d1, ordered=TRUE, estimator="ULS") 
  
  sco <- data.frame(lavPredict(scoresg2d1, method = "EBM"))
  
  scostd <- data.frame(sco)  
  
  factscores.g2d1 <- cbind(encg2d1$Carne, scostd)  
  colnames(factscores.g2d1) <- c("carne", "SA.1", "CO.1", "OE.1")
  
  #g2d2
  encg2d1 <- read.csv("enc2.g2.csv", header=T, sep=";")
  
  encg2d1[encg2d1=="1 (Totalmente en desacuerdo)"] <- 1
  encg2d1[encg2d1=="5 (Totalmente de acuerdo)"] <- 5
  
  m2a  <- ' SA  =~  AU12 + AU13 + AU11 + AU3  + AU10 + AU4
          CO =~   AU7 + AU8
          OE =~  AU6 + AU1 + AU2 '
  
  scoresg2d1 <- cfa(m2a, data=encg2d1, ordered=TRUE, estimator="ULS") 
  
  sco <- data.frame(lavPredict(scoresg2d1, method = "EBM"))
  
  scostd <- data.frame(sco)  
  
  factscores.g2d2 <- cbind(encg2d1$Carne, scostd)  
  
  colnames(factscores.g2d2) <- c("carne", "SA.2", "CO.2", "OE.2")
  
  #g2D3
  
  encg2d1 <- read.csv("enc3.g2.csv", header=T, sep=";")
  
  encg2d1[encg2d1=="1 (Totalmente en desacuerdo)"] <- 1
  encg2d1[encg2d1=="5 (Totalmente de acuerdo)"] <- 5
  
  m2a  <- ' SA  =~  AU12 + AU13 + AU11 + AU3  + AU10 + AU4
          CO =~   AU7 + AU8
          OE =~  AU6 + AU1 + AU2 '
  
  scoresg2d1 <- cfa(m2a, data=encg2d1, ordered=TRUE, estimator="ULS") 
  
  sco <- data.frame(lavPredict(scoresg2d1, method = "EBM"))
  
  scostd <- data.frame(sco)  
  
  factscores.g2d3 <- cbind(encg2d1$Carne, scostd)  
  
  colnames(factscores.g2d3) <- c("carne", "SA.3", "CO.3", "OE.3")
  
  
  
  
  factscores.g2 <- merge(factscores.g2d1, factscores.g2d2, by="carne")
  
  factscores.g2 <- merge(factscores.g2, factscores.g2d3, by="carne")
  
  factscores.g2 <- factscores.g2[!duplicated(factscores.g2$carne),]

#GROUP 6####

  #g6d1  
  encg6d1 <- read.csv("enc1.g6.csv", header=T, sep=";")
  
  encg6d1[encg6d1=="1 (Totalmente en desacuerdo)"] <- 1
  encg6d1[encg6d1=="5 (Totalmente de acuerdo)"] <- 5
  
  m2a  <- ' SA  =~  AU12 + AU13 + AU11 + AU3  + AU10 + AU4
          CO =~   AU7 + AU8
          OE =~  AU6 + AU1 + AU2 '
  
  scoresg6d1 <- cfa(m2a, data=encg6d1, ordered=TRUE, estimator="ULS") 
  
  sco <- data.frame(lavPredict(scoresg6d1, method = "EBM"))
  
  scostd <- data.frame(sco)  
  
  factscores.g6d1 <- cbind(encg6d1$Carne, scostd)  
  colnames(factscores.g6d1) <- c("carne", "SA.1", "CO.1", "OE.1")
  
  #g6d2
  encg6d1 <- read.csv("enc2.g6.csv", header=T, sep=";")
  
  encg6d1[encg6d1=="1 (Totalmente en desacuerdo)"] <- 1
  encg6d1[encg6d1=="5 (Totalmente de acuerdo)"] <- 5
  
  m2a  <- ' SA  =~  AU12 + AU13 + AU11 + AU3  + AU10 + AU4
          CO =~   AU7 + AU8
          OE =~  AU6 + AU1 + AU2 '
  
  scoresg6d1 <- cfa(m2a, data=encg6d1, ordered=TRUE, estimator="ULS") 
  
  sco <- data.frame(lavPredict(scoresg6d1, method = "EBM"))
  
  scostd <- data.frame(sco)  
  
  factscores.g6d2 <- cbind(encg6d1$Carne, scostd)  
  
  colnames(factscores.g6d2) <- c("carne", "SA.2", "CO.2", "OE.2")
  
  #g6D3
  
  encg6d1 <- read.csv("enc3.g6.csv", header=T, sep=";")
  
  encg6d1[encg6d1=="1 (Totalmente en desacuerdo)"] <- 1
  encg6d1[encg6d1=="5 (Totalmente de acuerdo)"] <- 5
  
  m2a  <- ' SA  =~  AU12 + AU13 + AU11 + AU3  + AU10 + AU4
          CO =~   AU7 + AU8
          OE =~  AU6 + AU1 + AU2 '
  
  scoresg6d1 <- cfa(m2a, data=encg6d1, ordered=TRUE, estimator="ULS") 
  
  sco <- data.frame(lavPredict(scoresg6d1, method = "EBM"))
  
  scostd <- data.frame(sco)  
  
  factscores.g6d3 <- cbind(encg6d1$Carne, scostd)  
  
  colnames(factscores.g6d3) <- c("carne", "SA.3", "CO.3", "OE.3")
  
  
  
  
  
  factscores.g6 <- merge(factscores.g6d1, factscores.g6d2, by="carne")
  
  factscores.g6 <- merge(factscores.g6, factscores.g6d3, by="carne")
  
  factscores.g6 <- factscores.g6[!duplicated(factscores.g6$carne),]

#GROUP 7####

  #g7d1  
  encg7d1 <- read.csv("enc1.g7.csv", header=T, sep=";")
  
  encg7d1[encg7d1=="1 (Totalmente en desacuerdo)"] <- 1
  encg7d1[encg7d1=="5 (Totalmente de acuerdo)"] <- 5
  
  m2a  <- ' SA  =~  AU12 + AU13 + AU11 + AU3  + AU10 + AU4
          CO =~   AU7 + AU8
          OE =~  AU6 + AU1 + AU2 '
  
  scoresg7d1 <- cfa(m2a, data=encg7d1, ordered=TRUE, estimator="ULS") 
  
  sco <- data.frame(lavPredict(scoresg7d1, method = "EBM"))
  scostd <- data.frame(sco)  
  factscores.g7d1 <- cbind(encg7d1$Carne, scostd)  
  colnames(factscores.g7d1) <- c("carne", "SA.1", "CO.1", "OE.1")
  
  #g7d2
  encg7d1 <- read.csv("enc2.g7.csv", header=T, sep=";")
  
  encg7d1[encg7d1=="1 (Totalmente en desacuerdo)"] <- 1
  encg7d1[encg7d1=="5 (Totalmente de acuerdo)"] <- 5
  
  m2a  <- ' SA  =~  AU12 + AU13 + AU11 + AU3  + AU10 + AU4
          CO =~   AU7 + AU8
          OE =~  AU6 + AU1 + AU2 '
  
  scoresg7d1 <- cfa(m2a, data=encg7d1, ordered=TRUE, estimator="ULS") 
  summary(scoresg7d1, std=T)
  
  lavInspect(scoresg7d1, "cov.lv")
  
  sco <- data.frame(lavPredict(scoresg7d1, method = "EBM"))
  
  scostd <- data.frame(sco)  
  
  factscores.g7d2 <- cbind(encg7d1$Carne, scostd)  
  
  colnames(factscores.g7d2) <- c("carne", "SA.2", "CO.2", "OE.2")
  
  #g7D3
  
  encg7d1 <- read.csv("enc3.g7.csv", header=T, sep=";")
  
  encg7d1[encg7d1=="1 (Totalmente en desacuerdo)"] <- 1
  encg7d1[encg7d1=="5 (Totalmente de acuerdo)"] <- 5
  
  m2a  <- ' SA  =~  AU12 + AU13 + AU11 + AU3  + AU10 + AU4
          CO =~   AU7 + AU8
          OE =~  AU6 + AU1 + AU2 '
  
  scoresg7d1 <- cfa(m2a, data=encg7d1, ordered=TRUE, estimator="ULS") 
  
  summary(scoresg7d1, std=T) ##The correlation between MI and MEI is above 0.9 but 
  ##the rest of correlations and parameters are ok so it seems 
  ## to be an outlier  
  
  lavInspect(scoresg7d1, "cov.lv")
  
  
  sco <- data.frame(lavPredict(scoresg7d1, method = "EBM"))
  
  scostd <- data.frame(sco)  
  
  factscores.g7d3 <- cbind(encg7d1$Carne, scostd)  
  
  colnames(factscores.g7d3) <- c("carne", "SA.3", "CO.3", "OE.3")
  
  
  
  
  
  factscores.g7 <- merge(factscores.g7d1, factscores.g7d2, by="carne")
  
  factscores.g7 <- merge(factscores.g7, factscores.g7d3, by="carne")
  
  
  factscores.g7 <- factscores.g7[!duplicated(factscores.g7$carne),]

ALL GROUPS ####
 group <- c(rep(1,73),rep(2, 47), rep(6, 71), rep(7,39))
  
  final.scores.ae <- data.frame(group, rbind(factscores.g1, factscores.g2, factscores.g6, factscores.g7))
  
  final.scores.ae  <- final.scores.ae [!duplicated(final.scores.ae $carne),]


### Uni then multivariate -> https://stats.stackexchange.com/questions/388827/what-is-the-point-of-univariate-regression-before-multivariate-regression
## https://www.youtube.com/watch?v=uvU-TmEt8_M&ab_channel=OlivierGimenez


INFO BÁSICA####
ib1 <-read.csv("info.basica.g1.csv", header=T, sep=";", fileEncoding="UTF-8-BOM")
ib2 <-read.csv("info.basica.g2.csv", header=T, sep=";",fileEncoding="UTF-8-BOM")
ib6 <-read.csv("info.basica.g6.csv", header=T, sep=";", fileEncoding="UTF-8-BOM")
ib7 <-read.csv("info.basica.g7.csv", header=T, sep=";", fileEncoding="UTF-8-BOM")
ib10 <-read.csv("info.basica.g10.csv", header=T, sep=";")

ib1267 <- rbind(ib1, ib2, ib6, ib7, ib10)


data.ib.qt <- merge( ib1267, final.scores.m, by="carne")

data.ib.qt <- merge( data.ib.qt, final.scores.ae, by="carne")

data.ib.qt <- data.ib.qt[!duplicated(data.ib.qt$carne),]


data.ib.qt$MO1 <- data.ib.qt$MI.1 + data.ib.qt$MEI.1 + data.ib.qt$MEE.1 + data.ib.qt$AM.1

data.ib.qt$MO2 <- data.ib.qt$MI.2 + data.ib.qt$MEI.2 + data.ib.qt$MEE.2 + data.ib.qt$AM.2

data.ib.qt$MO3 <- data.ib.qt$MI.3 + data.ib.qt$MEI.3 + data.ib.qt$MEE.3 + data.ib.qt$AM.3

data.ib.qt$AE1 <- data.ib.qt$SA.1 + data.ib.qt$CO.1 + data.ib.qt$OE.1

data.ib.qt$AE2 <- data.ib.qt$SA.2 + data.ib.qt$CO.2 + data.ib.qt$OE.2

data.ib.qt$AE3 <- data.ib.qt$SA.3 + data.ib.qt$CO.3 + data.ib.qt$OE.3


rowSums(data.ib.qt[,43:48] > 5)

data.ib.qt <- data.ib.qt[!rowSums(data.ib.qt[,43:48] > 5),]
data.ib.qt <- data.ib.qt[!rowSums(data.ib.qt[,43:48] < -5),]

write.csv(data.ib.qt,'data.ib.qt.csv')


NOTAS#####
notas.g1 <- read.csv("calificaciones.g1.csv", header=T, sep=";", dec=".")
notas.g1$grupo <- 1
notas.g2 <- read.csv("calificaciones.g2.csv", header=T, sep=";", dec=".")
notas.g2$grupo <- 2
notas.g6 <- read.csv("calificaciones.g6.csv", header=T, sep=";", dec=".")
notas.g6$grupo <- 6
notas.g7 <- read.csv("calificaciones.g7.csv", header=T, sep=";", dec=".")
notas.g7$grupo <- 7

cols <- 6:104
notas.g1[,cols]<-as.numeric(unlist(notas.g1[,cols]))
cols <- 6:81
notas.g2[,cols]<-as.numeric(unlist(notas.g2[,cols]))
cols <- 6:109
notas.g6[,cols]<-as.numeric(unlist(notas.g6[,cols]))
cols <- 6:81
notas.g7[,cols]<-as.numeric(unlist(notas.g7[,cols]))
str(notas.g1)

table(notas.g1$TC>=67.5)[[2]]/(table(notas.g1$TC>=67.5)[[1]]+table(notas.g1$TC>=67.5)[[2]]-table(notas.g1$TC<30)[[2]])
table(notas.g2$TC>=67.5)[[2]]/(table(notas.g2$TC>=67.5)[[1]]+table(notas.g2$TC>=67.5)[[2]]-table(notas.g2$TC<30)[[2]])
table(notas.g6$TC>=67.5)[[2]]/(table(notas.g6$TC>=67.5)[[1]]+table(notas.g6$TC>=67.5)[[2]]-table(notas.g6$TC<30)[[2]])
table(notas.g7$TC>=67.5)[[2]]/(table(notas.g7$TC>=67.5)[[1]]+table(notas.g7$TC>=67.5)[[2]]-table(notas.g7$TC<30)[[2]])
table(notas.g7$TC<30)[[2]]

notas.t <- merge(notas.g1, notas.g2, all=T)
notas.t <- merge(notas.t, notas.g6, all=T)
notas.t <- merge(notas.t, notas.g7, all=T)
notas.t <- notas.t[!is.na(notas.t$TC),]
notas.t <- data.frame(notas.t[notas.t$TC>30,])
notas.t$grupo <- as.factor(notas.t$grupo)
notas.t$nombre_completo <- paste(notas.t$Apellido.s., notas.t$ï..Nombre)

alumnos <- read.csv("MATRICULA.csv", sep=";")[,1:2]

notas.t <- merge(notas.t, alumnos, by="nombre_completo", all.x=T )
table(is.na(notas.t$ï..carne))
write.csv(notas.t, file="notas.t.csv")
notas.t <- read.csv("notas.t.csv", sep=";")
notas.t2 <- cbind(notas.t$ï..carne, notas.t$nota.t2, rep(2,488))
notas.t3 <- cbind(notas.t$ï..carne, notas.t$nota.t3, rep(3,488))
notas.red <- rbind(notas.t2, notas.t3)
colnames(notas.red) <- c("carne", "nota", "tiempo")

ANALISIS#####
data.ib.qt <- read.csv("data.ib.qt.csv", header=T, sep=",", dec=".")

  MO <- stack(data.ib.qt[,c(44,45)],)
  carne <- rep(data.ib.qt$carne,1)
  grupo <- rep(data.ib.qt$group.x,1)
 MO <- cbind(grupo, MO, carne)
  levels(MO$ind) <- c( 2, 3) 

  AE <- stack(data.ib.qt[,c(47,48)],)
  carne <- rep(data.ib.qt$carne,1)
  grupo <- rep(data.ib.qt$group.x,1)
  AE <- cbind(grupo, AE, carne)
  levels(AE$ind) <- c( 2, 3) 
  
  datt.comb <- Reduce(function(x,y) merge(x = x, y = y, by = c("carne","ind", "grupo")), 
                      list(MO, AE))
  
  colnames(datt.comb) <- c("carne", "tiempo", "grupo", "MO", "AE")

#for (i in 0:3) {
 
#datt <- stack(data.ib.qt[,c( 25+i, 29+i)],)
#carne <- rep(data.ib.qt$carne,2)
#grupo <- rep(data.ib.qt$group.x,2)
#datt <- cbind(grupo, datt, carne)
#levels(datt$ind) <- c( 2, 3) 

#assign(paste(strsplit(colnames(data.ib.qt[25+i]), "[.]")[[1]][1]), datt)
#}

#for (i in 0:2) {
  
  #datt <- stack(data.ib.qt[,c( 37+i, 40+i)],)
  #carne <- rep(data.ib.qt$carne,2)
 # grupo <- rep(data.ib.qt$group.x,2)
  #datt <- cbind(grupo, datt, carne)
  #levels(datt$ind) <- c( 2, 3) 
  
 # assign(paste(strsplit(colnames(data.ib.qt[34+i]), "[.]")[[1]][1]), datt)
#}

#datt.comb <- Reduce(function(x,y) merge(x = x, y = y, by = c("carne","ind", "grupo")), 
#      list(MI, MEI, MEE, AM, SA, OE, CO))

#colnames(datt.comb) <- c("carne", "tiempo", "grupo", "MI", "MEI", "MEE", "AM", "SA", "OE", "CO")


data.red <- merge( ib1267, data.ib.qt[,c(2,43,46)] , by="carne", all.y = T)
data.red <- merge( data.red, datt.comb , by="carne", all.y = T)

which(data.red=="C12177")
data.red <- data.red[-c(322,323,324),]

data.red$tratamiento <-as.factor(data.red$grupo)
levels(data.red$tratamiento) <- c("ER", "NORM", "ER", "NORM")
data.red <- cbind(data.red, tratamiento)



plot(density(data.red[data.red$tratamiento=="NORM",]$MO1))
par(new=TRUE)
plot(density(data.red[data.red$tratamiento=="ER",]$MO1))

table(data.red$tratamiento=="NORM")
length(data.red$tratamiento=="ER")

cols <- c(1, 2, 5:18, 20)

data.red[cols] <- lapply(data.red[cols], factor)

data.red <- as.data.frame(data.red)


str(data.red)


levels(data.red$area) <- c( "5 : CIENCIAS AGROALIMENTARIAS",
                            "1 : SALUD",
                            "2 : CIENCIAS BASICAS",         
                            "4 : INGENIERIAS",
                            "3 : SOCIALES", 
                            "2 : CIENCIAS BASICAS",         
                            "4 : INGENIERIAS",
                            "5 : CIENCIAS AGROALIMENTARIAS",
                            "1 : SALUD"  )

levels(data.red$colegio) <- c(  "1 : Científico",
                                "2 : Público",
                                "3 : Privado",
                                "4 : Técnico",
                                "5 : Semiprivado",
                                "5 : Semiprivado",
                                "3 : Privado",
                                "4 : Técnico",
                                "1 : Científico",
                                "2 : Público")

levels(data.red$conecti) <- c("1 : Alta. Me puedo conectar todo el tiempo y es muy estable",
                              "2 : Mala. Tengo limitaciones de conexión y es muy inestable",
                              "3 : Media. Me puedo conectar pero se pierde frecuentemente", 
                              "2 : Mala. Tengo limitaciones de conexión y es muy inestable",
                              "3 : Media. Me puedo conectar pero se pierde frecuentemente", 
                              "1 : Alta. Me puedo conectar todo el tiempo y es muy estable")

levels(data.red$tecnolog) <- c("1 : Computadora (o tablet) y teléfono celular",
                               "3 : Teléfono celular",                         
                               "2 : Computadora (o tablet)",
                               "1 : Computadora (o tablet) y teléfono celular",
                               "3 : Teléfono celular")      

data.red <- merge(data.red, notas.red, by=c("carne", "tiempo"), all.x=T)

write.csv(data.red, "data.red.csv")

Modelo#####

 BAYESIAN StanR######
# https://www.youtube.com/watch?v=j3aEvmeMNGY&ab_channel=ChristianGonzalez
# https://www.youtube.com/watch?v=FRs1iribZME&ab_channel=UseROslolibrary(brms)
# https://www.youtube.com/watch?v=1p_Us5WFQ7w&ab_channel=rasmusab
setwd("/Users/Sua/OneDrive/Tesis/Análisis/Experimento/")
data.red <- read.csv("data.red.csv", header=T, sep=";", )
data.red <- data.red[,-1]
cols <- c(1,2, 7:19, 23, 26)
data.red[cols] <- lapply(data.red[cols], factor)
data.red$tratamiento <- factor(data.red$tratamiento, levels=c("NORM","ER"))

fit0 <- readRDS("fit0.rds")
fit1 <- readRDS("fit1.rds")
fit2 <- readRDS("fit2.rds")
fit31 <- readRDS("fit3.rds")
fit4 <- readRDS("fit4.rds")
fit5 <- readRDS("fit5.rds")
fit51 <- readRDS("fit51.rds")
fit6 <- readRDS("fit6.rds")
fit2 <- readRDS("fitAE.trat.tiempo.carne.MO1.rds")
fit3 <- readRDS("fitMO.trat.tiempo.tratxtiempo.carne.MO1.rds")
#Modelo completo MO#####
fit <- lmer( MO~ tratamiento + tiempo + tratamiento*tiempo + MO1 -1 + (1|carne), data= data.red)

fit0 <- brm( MO~ tratamiento + tiempo + tratamiento*tiempo + (1|MO1) 
             + (1|grupo) + (1|prom_adm) + (1|trabajo) + (1|carrera)
             + (1|colegio) + (1|gustabio) + (1|carne)-1,
             family=gaussian(), chains=4, cores=4, data= data.red, iter=15000, warmup= 3000,
             save_pars = save_pars(all = TRUE))

saveRDS(fit0, "fit0.rds")

fit0 <- brm( MO~ 1,
             family=gaussian(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
             save_pars = save_pars(all = TRUE))

fit1 <- brm( MO~ tratamiento,
             family=gaussian(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
             save_pars = save_pars(all = TRUE))
saveRDS(fit1, "fit1.rds")


fit2 <- brm( MO~ tratamiento + tiempo + MO1 -1 + (1|carne),
              family=gaussian(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
              save_pars = save_pars(all = TRUE))
saveRDS(fit3, "fit3.rds")


fit3 <- brm( MO~ tratamiento + tiempo + tratamiento*tiempo + MO1 -1 + (1|carne),
             family=gaussian(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
             save_pars = save_pars(all = TRUE))

get_prior( MO~ tratamiento + tiempo + tratamiento*tiempo + MO1 -1 + (1|carne),
          data = data.red, family = gaussian())

bprior <- c(prior_string("normal(0,10)", class = "b"),
            prior(normal(0,2), class = b, coef = MO1),
            prior(normal(0,2), class = b, coef = tratamientoER),
            prior(normal(0,2), class = b, coef = tratamientoNORM),
            prior_(~cauchy(0,2), class = ~sd, 
                   group = ~carne, coef = ~Intercept))

fit31 <- brm( MO~ tratamiento + tiempo + tratamiento*tiempo + MO1 -1 + (1|carne),
             family=gaussian(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
             save_pars = save_pars(all = TRUE), prior = bprior)


fit32 <- brm( MO~ tratamiento + tiempo + tratamiento*tiempo -1 + (1|carne)+ (1|colegio) ,
             family=gaussian(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
             save_pars = save_pars(all = TRUE))


fit4 <- brm( MO~ tratamiento + tiempo + tratamiento*tiempo + MO1 -1 + (1|carne) + (1|prom_adm),
             family=gaussian(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
             save_pars = save_pars(all = TRUE))

loo(fit0, fit1, fit2, fit3, fit32, fit31)


saveRDS(fit2, "fitAE.trat.tiempo.carne.MO1.rds")


bprior <- c(prior_string("normal(0,3)", class = "b"))

bform <- bf(MO~ tratamiento + tiempo + tratamiento*tiempo -1  + MO1 + (1|ID1|carne),
            sigma~ tratamiento + tiempo + tratamiento*tiempo -1  + MO1 + (1|ID1|carne))

fit5 <- brm(bform,chains=4, cores=4, data= data.red, iter=5000, warmup= 2000, family= gaussian(),
            save_pars = save_pars(all = TRUE))

bform <- bf(MO~ tratamiento + tiempo + tratamiento*tiempo -1  + (1|ID1|carne),
            sigma~ tratamiento + tiempo + tratamiento*tiempo -1  + (1|ID1|carne))

fit51 <- brm(bform,chains=4, cores=4, data= data.red, iter=5000, warmup= 2000, family= gaussian(),
            save_pars = save_pars(all = TRUE), prior=bprior)


loo.MO.tr.T.MO1.trxT.carne <- loo(fit5,moment_match = TRUE, cores=4)
kfold.MO.tr.T.MO1.trxT.carne <- kfold(fit5,moment_match = TRUE, cores=4)
saveRDS(loo.MO.tr.T.MO1.trxT.carne, "loo.MO.tr.T.MO1.trxT.carne.rds")


loofit5 <- LOO(fit31, fit3)
loofit3 <- LOO(fit5, moment_match = TRUE, cores=3)
kfit5 <- kfold(fit5, K=10, cores=4)
kfit3 <- kfold(fit3, K=10, cores=4)
compare(kfit5, kfit3)
saveRDS(kfit5, "kfit5.rds")
saveRDS(kfit3, "kfit3.rds")
pareto-k-diagnostic
launch_shinystan(fit5)


#Modelo completo AE####
plot(density(data.red$AE))

fit <- lmer( AE~ tratamiento + tiempo + tratamiento*tiempo + AE1 -1 + (1|carne), data= data.red)
summary(fit)
fit0 <- brm( AE~ 1,
             family=gaussian(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
             save_pars = save_pars(all = TRUE))

fit1 <- brm( AE~ tratamiento,
             family=gaussian(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
             save_pars = save_pars(all = TRUE))
saveRDS(fit1, "fit1.rds")


fit2 <- brm( AE~ tratamiento + tiempo + AE1 -1 + (1|carne),
             family=gaussian(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
             save_pars = save_pars(all = TRUE))
saveRDS(fit3, "fit3.rds")


fit3 <- brm( AE~ tratamiento + tiempo + tratamiento*tiempo + AE1 -1 + (1|carne),
             family=gaussian(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
             save_pars = save_pars(all = TRUE))

loo(fit2, fit3)
saveRDS(fit5, "fit5AE.rds")

bprior <- c(prior_string("normal(0,10)", class = "b"),
            prior(normal(0,2), class = b, coef = AE1),
            prior(normal(0,2), class = b, coef = tratamientoER),
            prior(normal(0,2), class = b, coef = tratamientoNORM),
            prior_(~cauchy(0,2), class = ~sd, 
                   group = ~carne, coef = ~Intercept))

fit31 <- brm( AE~ tratamiento + tiempo + AE1 -1 + (1|carne),
               family=gaussian(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
               save_pars = save_pars(all = TRUE), prior=bprior)


fit4 <- brm( AE~ tratamiento + tiempo + tratamiento*tiempo + AE1 -1 + (1|carne) + (1|grupo),
             family=gaussian(), chains=4, cores=4, data= data.red, iter=15000, warmup= 10000,
             save_pars = save_pars(all = TRUE))

summary(fit5)
conditional_effects(fit3)
loo(fit2, fit5)
pairs(fit5)

bform <- bf(AE~  tratamiento + tiempo +  -1  + AE1 + (1|ID1|carne),
            sigma~  tratamiento + tiempo  -1  + AE1 + (1|ID1|carne))

fit5AE <- brm(bform,chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
            save_pars = save_pars(all = TRUE), prior= bprior)

loofitAE <- LOO(fit3, fit4, fit5, cores=4)
pp_check(fit5)
loofitAE5 <- LOO( fit5, cores=4, moment_match = TRUE)
kfit5 <- kfold(fit5, K=10, cores=4)
kfit3 <- kfold(fit3, K=10, cores=4)
compare(kfit5, kfit3)
summary(fit5)

conditional_effects(fit5, conditions = data.frame(tiempo = 3))

kfit5 <- kfold(fit5, K=10, cores=4)
saveRDS(kfit5, "kfit5.rds")

kfit5AE <- kfold(fit5AE, K=10, cores=4)
saveRDS(kfit5AE, "kfitAE5.rds")

kfit3 <- kfold(fit3, K=10, cores=4)
saveRDS(kfit3, "kfit3.rds")

kfit2AE <- kfold(fit2, K=10, cores=4)
saveRDS(kfit2AE, "kfitAE2.rds")

kfit5 <- readRDS("kfit5.rds")

loo_compare(kfit5, kfit3)

#Modelo completo notas####
fit3 <- readRDS("fit3nota.rds")
plot(density(data.red$nota))
data.red <- data.red[data.red$nota!=0,]
data.red$nota <- data.red$nota/100
summary(data.red$nota)
fit <- lmer( nota~ tratamiento + tiempo + tratamiento*tiempo +MO1  -1 + (1|carne), data= data.red)
summary(fit5)

fit0 <- brm( AE~ tratamiento + tiempo + tratamiento*tiempo + (1|AE1) 
             + (1|grupo) + (1|prom_adm) + (1|trabajo) + (1|carrera)
             + (1|colegio) + (1|gustabio) + (1|carne)-1,
             family=skew_normal(), chains=4, cores=4, data= data.red, iter=15000, warmup= 3000,
             save_pars = save_pars(all = TRUE))

saveRDS(fit0, "fit0.rds")


fit0 <- brm( nota~ 1,
             family=skew_normal(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
             save_pars = save_pars(all = TRUE))
saveRDS(fit1, "fit1.rds")


fit1 <- brm( nota~ tratamiento -1 + (1|carne),
             family=gaussian(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
             save_pars = save_pars(all = TRUE))
#correr lot este
fit2<- brm( nota~ tratamiento + tiempo -1 + (1|carne),
               family=Beta(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
               save_pars = save_pars(all = TRUE))

#correr este
fit21<- brm( nota~ tratamiento + tiempo -1 + (1|carne),
            family=skew_normal(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
            save_pars = save_pars(all = TRUE))

fit3<- brm( nota~ tratamiento + tiempo + tratamiento*tiempo -1 + (1|carne),
             family=Beta(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
             save_pars = save_pars(all = TRUE))

fit3not3 <- brm( nota~ tratamiento + tiempo + tratamiento*tiempo -1 + (1|carne),
                 family=skew_normal(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
                 save_pars = save_pars(all = TRUE))

fitnot32 <- brm( nota~ tratamiento + tiempo + tratamiento*tiempo  -1 + (1|carne),
             family=Beta("cauchit"), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
             save_pars = save_pars(all = TRUE))

conditional_effects(fit3not3)
loo(fit3)
loofit <- loo(fit2, moment_match = TRUE, cores=3)
saveRDS(loofit, "loofit2nota.sin.inter.rds")
loofit <- loo(fit3, moment_match = TRUE)
saveRDS(loofit, "loofit3nota.sin.inter.rds")
loofit2nota <- readRDS( "loofit2nota.sin.inter.rds")
loofit3nota <- readRDS( "loofit3nota.sin.inter.rds")
loo_compare(loofit3nota, loofit2nota)

str(data.red)
summary(fitnot32)
loo(fitnot32)

loonot3 <- LOO( fitnot3, cores=3, moment_match = TRUE)


loo(fit2)
summary(fit1)
pp_check(fit1)
loo(fitnot3)

conditional_effects(fit2)

#, conditions = data.frame(tiempo = 3)

bprior <- c(prior_string("normal(0,1000)", class = "b"))
fit32 <- brm( nota~ tratamiento + tiempo -1 +  (1|carne),
             family=Beta(), chains=4, cores=4, data= data.red, iter=5000, warmup= 2000,
             save_pars = save_pars(all = TRUE))
pp_check(fit32)
summary(fit2)
conditional_effects(fit31)[[1]]
pairs(fit4)

bform <- bf(nota~ tratamiento + tiempo -1  + (1|ID1|carne),
            sigma~ tratamiento + tiempo  -1   + (1|ID1|carne))

fit5 <- brm(bform,chains=4, cores=4, data= data.red, iter=7000, warmup= 2000,
            save_pars = save_pars(all = TRUE),family=skew_normal())
pairs(fit5)
loofit <- LOO(fit3, fit2, cores=4)

loofitAE5 <- LOO( fit5, cores=4, moment_match = TRUE)
kfit5 <- kfold(fit2, K=10, cores=4)

kfit3 <- kfold(fit31, K=10, cores=4)
compare(kfit5, kfit3)
summary(fit5)

conditional_effects(fit3, conditions = data.frame(tiempo = 3))

saveRDS(kfit3, "kfit31nota.rds")

#PRUEBA ####
#https://cran.r-project.org/web/packages/tidybayes/vignettes/tidy-brms.html
setwd("/Users/Sua/OneDrive/Tesis/Análisis/Experimento/")

GRAFICOSMO #####
setwd("/Users/Sua/Documents/modelos/")
fit5 <- readRDS("fitMO.trat.tiempo.tratxtiempo.carne.MO1.sigma.rds")

ER <- spread_draws(fit5,b_tratamientoER)$b_tratamientoER
NORM <- spread_draws(fit5,b_tratamientoNORM)$b_tratamientoNORM
tiempo <- spread_draws(fit5,b_tiempo3)$b_tiempo3
MO1 <- spread_draws(fit5,b_MO1)$b_MO1
carne <- spread_draws(fit5,sd_carne__Intercept)$sd_carne__Intercept
tratxtiempo <- -spread_draws(fit5,`b_tratamientoNORM:tiempo3`)$`b_tratamientoNORM:tiempo3`
sigER <- 10^spread_draws(fit5,b_sigma_tratamientoER)$b_sigma_tratamientoER
sigNORM <- 10^spread_draws(fit5,b_sigma_tratamientoNORM)$b_sigma_tratamientoNORM
sigtratxtiempo <- 10^-spread_draws(fit5,`b_sigma_tratamientoNORM:tiempo3`)$`b_sigma_tratamientoNORM:tiempo3`
sigMO1 <- 10^spread_draws(fit5,b_sigma_MO1)$b_sigma_MO1
sigcarne <- 10^spread_draws(fit5,sd_carne__sigma_Intercept)$sd_carne__sigma_Intercept
sigtiempo <- 10^spread_draws(fit5,b_sigma_tiempo3)$b_sigma_tiempo3

condEff <- data.frame(conditional_effects(fit5, conditions = data.frame(tiempo = 2), points=T)[[1]])
condEff2 <- data.frame(conditional_effects(fit5, conditions = data.frame(tiempo = 3))[[1]])
condEff3 <- rbind(condEff,condEff2)

#Graph efectos marginales boxplot
g4 <-ggplot(condEff3, aes(x=tiempo, y=estimate__, group=tratamiento, color=tratamiento)) + 
  geom_point(size=5,position=position_dodge(0.5))+
  theme_bw() +  
  geom_errorbar(aes(ymin=lower__, ymax=upper__), width=.15,
                position=position_dodge(0.5), size=1.25) +
  theme(axis.line = element_line(colour = "black"),
        legend.title= element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
       # panel.border = element_blank(),
        panel.background = element_blank(),
       legend.justification = c("right", "top"))+
  labs( x="T", y = "Autodeterminación")+
scale_fill_manual(values = c("#00798c", "#d1495b")) +
  scale_color_manual(values = c("#00798c", "#d1495b")) 


graphdat <- data.frame(ER, sigER, NORM, sigNORM, tiempo, sigtiempo, MO1, sigMO1, carne, sigcarne, tratxtiempo,sigtratxtiempo)
graphdat <- melt(graphdat)
graphdat2 <- aggregate(value~variable, data=graphdat, quantile, p=c(0.025,0.1,0.5,0.90,0.975))
graphdat3 <- as.data.frame(cbind(levels(graphdat2$variable),graphdat2$value))
graphdat3[,2:6] <- as.numeric(unlist(graphdat3[,2:6]))
colnames(graphdat3) <- c("variable", "l.2.5", "l.05", "l.50", "l.95", "l.97.5")

graphdat3$colors <- as.factor(c(1,1,2,2,3,3,4,4,5,5,6,6))
graphdat3$variable <- c("ER","Var ER","NORM","Var NORM","T3","Var T3","LB","Var LB", "ID","Var ID", "ER:T3", "Var ER:T3")
graphdat3$variable <- factor(graphdat3$variable, levels = c("Var ID", "ID", 
                                                            "Var ER:T3", "ER:T3", 
                                                            "Var T3", "T3", 
                                                            "Var LB", "LB",
                                                            "Var NORM", "NORM",
                                                            "Var ER", "ER"  ))

g.est.MO <- ggplot(data = graphdat3, aes(x = variable, y = l.50, colour = colors)) +
  geom_point(position = position_dodge(width = 0.2), size= 2.5)  +
  coord_flip() +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.3, data= graphdat3, aes( ymin = l.2.5, ymax = l.97.5)) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0, size= 1.5, data= graphdat3, aes( ymin = l.05, ymax = l.95)) +
  scale_color_manual(values = c("#00798c", "#d1495b", "#66a182", "#50B9AB", "#FBB13C", "#725A6F"))+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  theme_classic()+
  labs( x="", y = "Autodeterminación")+
  theme(legend.position= "none")

#Graph estimados distribución 

graphdat <- data.frame(ER, sigER, NORM, sigNORM, tiempo, sigtiempo, MO1, sigMO1, carne, sigcarne, tratxtiempo,sigtratxtiempo)
colnames(graphdat) <- c("ER","Var ER","NORM","Var NORM","T3","Var T3","LB","Var LB", "Estudiante","Var Est.", "ER:T3", "Var ER:T3")
graphdat <- melt(graphdat)
graphdat$variable <- factor(graphdat$variable, levels = c("Var Est.", "Estudiante", 
                                                            "Var ER:T3", "ER:T3", 
                                                            "Var T3", "T3", 
                                                            "Var LB", "LB",
                                                            "Var NORM", "NORM",
                                                            "Var ER", "ER"  ))

ggplot(graphdat, aes(x = value, y = variable, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025,0.1,0.9, 0.975)) +
  scale_x_continuous(limits = c(-1.2, 4.5), expand = c(0, 0))+
  scale_fill_manual(
    name = "Probability", values = c("#77D3E1","#3FA9B9","#148393","#3FA9B9" ,"#77D3E1"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]"))+
   theme_ridges()

#Graph efecto marginales distribución 
newdata <- data.frame(
  tiempo = factor("2"),
  MO = -0.01635971,
  MO1 = 0.009224904,
  tratamiento = factor(c("ER", "NORM")))
pp <- data.frame(posterior_predict(fit5, newdata = newdata, re_formula=NA))
colnames(pp) <- c("ER.T2", "NORM.T2")
newdata <- data.frame(
  tiempo = factor("3"),
  MO = -0.01635971,
  MO1 = 0.009224904,
  tratamiento = factor(c("ER", "NORM")))

pp2 <- data.frame(posterior_predict(fit5, newdata = newdata, re_formula=NA))
colnames(pp2) <- c("ER.T3", "NORM.T3")
PP <- melt(cbind(pp,pp2))
PP$variable <- factor(PP$variable, levels = c("NORM.T3", "NORM.T2", "ER.T3", "ER.T2"))

graphpp <- ggplot(PP, aes(x = value, y = variable, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025,0.1,0.9, 0.975)) +
  scale_x_continuous(limits = c(-4.5, 4.5), expand = c(0, 0))+
  scale_fill_manual(
    name = "Probability", values = c("#77D3E1","#3FA9B9","#148393","#3FA9B9" ,"#77D3E1"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]"))+
  theme_ridges()


GRAFICOS_AE #####
fit5 <- readRDS("fitAE.trat.tiempo.carne.MO1.sigma.rds")

ER <- spread_draws(fit5,b_tratamientoER)$b_tratamientoER
NORM <- spread_draws(fit5,b_tratamientoNORM)$b_tratamientoNORM
tiempo <- spread_draws(fit5,b_tiempo3)$b_tiempo3
AE1 <- spread_draws(fit5,b_AE1)$b_AE1
carne <- spread_draws(fit5,sd_carne__Intercept)$sd_carne__Intercept
sigER <- 10^spread_draws(fit5,b_sigma_tratamientoER)$b_sigma_tratamientoER
sigNORM <- 10^spread_draws(fit5,b_sigma_tratamientoNORM)$b_sigma_tratamientoNORM
sigAE1 <- 10^spread_draws(fit5,b_sigma_AE1)$b_sigma_AE1
sigcarne <- 10^spread_draws(fit5,sd_carne__sigma_Intercept)$sd_carne__sigma_Intercept
sigtiempo <- 10^spread_draws(fit5,b_sigma_tiempo3)$b_sigma_tiempo3

#graph efectos marginales boxplot

condEff <- data.frame(conditional_effects(fit5, conditions = data.frame(tiempo = 2))[[1]])
condEff2 <- data.frame(conditional_effects(fit5, conditions = data.frame(tiempo = 3))[[1]])
condEff3 <- rbind(condEff,condEff2)


g44 <-ggplot(condEff3, aes(x=tiempo, y=estimate__, group=tratamiento, color=tratamiento)) + 
  geom_point(size=5,position=position_dodge(0.5))+
  theme_bw() +  
  geom_errorbar(aes(ymin=lower__, ymax=upper__), width=.15,
                position=position_dodge(0.5), size=1.25) +
  theme(axis.line = element_line(colour = "black"),
        legend.title= element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_blank(),
        panel.background = element_blank(),
        legend.justification = c("right", "top"))+
  labs( x="T", y = "Autoevaluación")+
  scale_fill_manual(values = c("#00798c", "#d1495b")) +
  scale_color_manual(values = c("#00798c", "#d1495b")) 




graphdat <- data.frame(ER, sigER, NORM, sigNORM, tiempo, sigtiempo, AE1, sigAE1, carne, sigcarne)
graphdat <- melt(graphdat)
graphdat2 <- aggregate(value~variable, data=graphdat, quantile, p=c(0.025,0.075,0.5,0.925,0.975))
graphdat3 <- as.data.frame(cbind(levels(graphdat2$variable),graphdat2$value))
graphdat3[,2:6] <- as.numeric(unlist(graphdat3[,2:6]))
colnames(graphdat3) <- c("variable", "l.2.5", "l.05", "l.50", "l.95", "l.97.5")
graphdat3$colors <- as.factor(c(1,1,2,2,3,3,4,4,5,5))

graphdat3$variable <- c("ER","Var ER","NORM","Var NORM","T3","Var T3","LB","Var LB", "ID","Var ID")
graphdat3$variable <- factor(graphdat3$variable, levels = c("Var ID", "ID", 
                                                            "Var T3", "T3", 
                                                            "Var LB", "LB",
                                                            "Var NORM", "NORM",
                                                            "Var ER", "ER"  ))

g.est.AE <- ggplot(data = graphdat3, aes(x = variable, y = l.50, colour = colors)) +
  geom_point(position = position_dodge(width = 0.2), size= 2.5)  +
  coord_flip() +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.3, data= graphdat3, aes( ymin = l.2.5, ymax = l.97.5)) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0, size= 1.5, data= graphdat3, aes( ymin = l.05, ymax = l.95)) +
  scale_color_manual(values = c("#00798c", "#d1495b", "#66a182", "#50B9AB", "#FBB13C"))+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  theme_classic()+
  labs( x="", y = "Autoevaluación")+
  theme(legend.position= "none")

#Graph estimados distribución 

graphdat <- data.frame(ER, sigER, NORM, sigNORM, tiempo, sigtiempo, MO1, sigMO1, carne, sigcarne)
colnames(graphdat) <- c("ER","Var ER","NORM","Var NORM","T3","Var T3","LB","Var LB", "Estudiante","Var Est.")
graphdat <- melt(graphdat)
graphdat$variable <- factor(graphdat$variable, levels = c("Var Est.", "Estudiante", 
                                                          "Var T3", "T3", 
                                                          "Var LB", "LB",
                                                          "Var NORM", "NORM",
                                                          "Var ER", "ER"  ))

ggplot(graphdat, aes(x = value, y = variable, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025,0.1,0.9, 0.975)) +
  scale_x_continuous(limits = c(-1.2, 4.5), expand = c(0, 0))+
  scale_fill_manual(
    name = "Probability", values = c("#77D3E1","#3FA9B9","#148393","#3FA9B9" ,"#77D3E1"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]"))+
  theme_ridges()

#Graph efecto marginales distribución 
newdata <- data.frame(
  tiempo = factor("2"),
  MO = -0.01635971,
  MO1 = 0.009224904,
  tratamiento = factor(c("ER", "NORM")))
pp <- data.frame(posterior_predict(fit5, newdata = newdata, re_formula=NA))
colnames(pp) <- c("ER.T2", "NORM.T2")
newdata <- data.frame(
  tiempo = factor("3"),
  MO = -0.01635971,
  MO1 = 0.009224904,
  tratamiento = factor(c("ER", "NORM")))

pp2 <- data.frame(posterior_predict(fit5, newdata = newdata, re_formula=NA))
colnames(pp2) <- c("ER.T3", "NORM.T3")
PP <- melt(cbind(pp,pp2))
PP$variable <- factor(PP$variable, levels = c("NORM.T3", "NORM.T2", "ER.T3", "ER.T2"))

graphpp <- ggplot(PP, aes(x = value, y = variable, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025,0.1,0.9, 0.975)) +
  scale_x_continuous(limits = c(-4.5, 4.5), expand = c(0, 0))+
  scale_fill_manual(
    name = "Probability", values = c("#77D3E1","#3FA9B9","#148393","#3FA9B9" ,"#77D3E1"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]"))+
  theme_ridges()



GRAFICOS_nota #####

fit5 <- readRDS("fitnota.trat.tiempo.carne.rds")

ER <- spread_draws(fit5,b_tratamientoER)$b_tratamientoER
NORM <- spread_draws(fit5,b_tratamientoNORM)$b_tratamientoNORM
tiempo <- spread_draws(fit5,b_tiempo3)$b_tiempo3
carne <- spread_draws(fit5,sd_carne__Intercept)$sd_carne__Intercept


condEff <- data.frame(conditional_effects(fit5, conditions = data.frame(tiempo = 2))[[1]])
condEff2 <- data.frame(conditional_effects(fit5, conditions = data.frame(tiempo = 3))[[1]])
condEff3 <- rbind(condEff,condEff2)


g444 <-ggplot(condEff3, aes(x=tiempo, y=estimate__, group=tratamiento, color=tratamiento)) + 
  geom_point(size=5,position=position_dodge(0.5))+
  theme_bw() +  
  geom_errorbar(aes(ymin=lower__, ymax=upper__), width=.15,
                position=position_dodge(0.5), size=1.25) +
  theme(axis.line = element_line(colour = "black"),
        legend.title= element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        # panel.border = element_blank(),
        panel.background = element_blank(),
        legend.justification = c("right", "top"))+
  labs( x="T", y = "Rendimiento")+
  scale_fill_manual(values = c("#00798c", "#d1495b")) +
  scale_color_manual(values = c("#00798c", "#d1495b")) 

graphdat <- data.frame(ER, NORM,  tiempo,  carne)
graphdat <- melt(graphdat)
graphdat2 <- aggregate(value~variable, data=graphdat, quantile, p=c(0.025,0.075,0.5,0.925,0.975))
graphdat3 <- as.data.frame(cbind(levels(graphdat2$variable),graphdat2$value))
graphdat3[,2:6] <- as.numeric(unlist(graphdat3[,2:6]))
colnames(graphdat3) <- c("variable", "l.2.5", "l.05", "l.50", "l.95", "l.97.5")

graphdat3$variable <- c("ER","NORM","T3", "ID")
graphdat3$variable <- factor(graphdat3$variable, levels = c( "ID", 
                                                            "T3", 
                                                            "NORM",
                                                            "ER"  ))
graphdat3$colors <- as.factor(c(1,2,3,4))

g.est.nota <- ggplot(data = graphdat3, aes(x = variable, y = l.50, colour = colors)) +
  geom_point(position = position_dodge(width = 0.2), size= 2.5)  +
  coord_flip() +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0.3, data= graphdat3, aes( ymin = l.2.5, ymax = l.97.5)) +
  geom_errorbar(position = position_dodge(width = 0.2), width = 0, size= 1.5, data= graphdat3, aes( ymin = l.05, ymax = l.95)) +
  scale_color_manual(values = c("#00798c", "#d1495b", "#66a182", "#FBB13C"))+
  geom_hline(yintercept=0, linetype="dashed", color = "red")+
  theme_classic()+
  labs( x="", y = "Rendimiento")+
  theme(legend.position= "none")

ggarrange(g.est.MO, g.est.AE,g.est.nota, labels = c("A.", "B.", "C."), ncol=1, align="v")

#Graph estimados distribución 

graphdat <- data.frame(ER, sigER, NORM, sigNORM, tiempo, sigtiempo, MO1, sigMO1, carne, sigcarne, tratxtiempo,sigtratxtiempo)
colnames(graphdat) <- c("ER","NORM", "T3", "Estudiante")
graphdat <- melt(graphdat)
graphdat$variable <- factor(graphdat$variable, levels = c("Estudiante", 
                                                          "T3", 
                                                         "NORM",
                                                          "ER"  ))

ggplot(graphdat, aes(x = value, y = variable, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025,0.1,0.9, 0.975)) +
  scale_x_continuous(limits = c(-1.2, 4.5), expand = c(0, 0))+
  scale_fill_manual(
    name = "Probability", values = c("#77D3E1","#3FA9B9","#148393","#3FA9B9" ,"#77D3E1"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]"))+
  theme_ridges()

#Graph efecto marginales distribución 
newdata <- data.frame(
  tiempo = factor("2"),
  MO = -0.01635971,
  MO1 = 0.009224904,
  tratamiento = factor(c("ER", "NORM")))
pp <- data.frame(posterior_predict(fit5, newdata = newdata, re_formula=NA))
colnames(pp) <- c("ER.T2", "NORM.T2")
newdata <- data.frame(
  tiempo = factor("3"),
  MO = -0.01635971,
  MO1 = 0.009224904,
  tratamiento = factor(c("ER", "NORM")))

pp2 <- data.frame(posterior_predict(fit5, newdata = newdata, re_formula=NA))
colnames(pp2) <- c("ER.T3", "NORM.T3")
PP <- melt(cbind(pp,pp2))
PP$variable <- factor(PP$variable, levels = c("NORM.T3", "NORM.T2", "ER.T3", "ER.T2"))

graphpp <- ggplot(PP, aes(x = value, y = variable, fill = factor(stat(quantile)))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    calc_ecdf = TRUE,
    quantiles = c(0.025,0.1,0.9, 0.975)) +
  scale_x_continuous(limits = c(-4.5, 4.5), expand = c(0, 0))+
  scale_fill_manual(
    name = "Probability", values = c("#77D3E1","#3FA9B9","#148393","#3FA9B9" ,"#77D3E1"),
    labels = c("(0, 0.025]", "(0.025, 0.975]", "(0.975, 1]"))+
  theme_ridges()





g3 <- g3 +
  theme(plot.margin = unit(c(1,0.5,0,0), "cm")) 
g33 <- g33 +
  theme(plot.margin = unit(c(1,0.5,0,0), "cm"))
g333 <- g333 +
  theme(plot.margin = unit(c(1,0.5,0,0), "cm"))
ggarrange(g3, g33, g333, labels = c("A.", "B.", "C."), ncol=3, common.legend = TRUE, legend="none")

g4 <- g4 +
  theme(plot.margin = unit(c(1,0.5,0.5,0), "cm")) 
g44 <- g44 +
  theme(plot.margin = unit(c(1,0.5,0.5,0), "cm"))
g444 <- g444 +
  theme(plot.margin = unit(c(1,0.5,0,0), "cm"))


ggarrange(g4+ rremove("xlab") ,g44+ rremove("xlab"),g444, labels = c("A.","B.", "C."), ncol=1, nrow=3, align="v", common.legend = TRUE)
