#####################
# objects_outils.R  #
#####################

#chargement des librairies
library(psy)
library(ggplot2)
library(dplyr)
library(gridExtra) #pour mettre plusieurs plot sur une même page
library(knitr) #pour faire de "belles" tables
library(stringr)
library(survival) #pour les analyses de survie
library(boot) #pour le bootstrap
library(lme4) #pour le modele mixte
library(GGally) #pour faire de beaux plot de survie
#library(survminer) #autre package pour faire de belles courbes de survie, mais moins modifiables qu'avec GGaly
library(pbkrtest) #pour estimer le nombre de degrés de libertés d'un modele mixte


#chargement des objets
dw <- readRDS("data/dw.rds")
dl <- readRDS("data/dl.rds")
dl0 <- readRDS("data/dl0.rds")  #items hamilton Q1
dl56 <- readRDS("data/dl56.rds")#items hamilron Q1
sc0 <- readRDS("data/sc0.rds")  #items scl90 Q1
sc56 <- readRDS("data/sc56.rds")#items scl90 Q1
dwh <- readRDS("data/dwh.rds")  #scores de hamilton aux différents temps (Q2 LOCF)
dlh <- readRDS("data/dlh.rds")  #scores de hamilton en format long (Q2 modele mixte)
dws <- readRDS("data/dws.rds")  #données de survie (Q3)


#Dimensions de scl90
dimensions <- c("somatisation","symptobs","sensitivite","depression","anxiete","hostilite","phobie","parano","psychotique")
somatisation <- paste0("Q", c(1,4,12,27,42,48,49,52,53,56,58,40))
symptobs <- paste0("Q", c(9,10,28,38,3,4,46,51,55,65))
sensitivite <- paste0("Q", c(6,21,34,36,37,41,61,69,73))
depression <- paste0("Q", c(5,14,15,20,22,26,29,30,31,32,54,71,79))
anxiete <- paste0("Q", c(2,17,23,33,39,57,72,78,80,86))
hostilite <- paste0("Q", c(11,24,63,67,74,81))
phobie <- paste0("Q", c(13,25,47,70,75,82,50))
parano <- paste0("Q", c(8,18,43,68,76,83))
psychotique <- paste0("Q",c(7,16,35,62,77,84,85,87,90,88))
#divers <- paste0("Q",c(19,44,59,60,64,66,89))


