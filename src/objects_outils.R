library(psy)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(knitr)
library(stringr)
library(survival)
library(boot)
library(lme4)
library(GGally) #pour faire de beaux plot de survie
library(survminer) #autre package pour faire de belles courbes de survie

#gpe <- read.csv2("data/groupe.csv") #utilisé dans calcul des scores globaux façon 2
dw <- readRDS("data/dw.rds")
dl <- readRDS("data/dl.rds")
dwh <- readRDS("data/dwh.rds") #ne contient que les scores globaux de hamilton aux différents temps (utile pour Q2 LOCF et Q3)
dlh <- readRDS("data/dlh.rds") #contient les scores globaux, les temps NUMERO et groupe en format long
dws <- readRDS("data/dws.rds")

vartime <-   c(0, 4, 7, 14, 21, 28, 42, 56)

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


#Dataset hamilton et scl90 aux temps 0 et 56:
dl0 <- dl %>% filter(time==0) %>% select(NUMERO, grep("HAM",colnames(dl)))
#h0 <- dl %>% filter(time==0) %>% select(NUMERO, grep("HAM",colnames(dl)))
sc0 <- dl %>% filter (time==0) %>% select(NUMERO, grep("Q", colnames(dl)))

dl56 <- dl %>% filter(time==56) %>% select(NUMERO, grep("HAM",colnames(dl)))
#h56 <- dl %>% filter(time==56) %>% select(NUMERO, grep("HAM",colnames(dl)))
sc56 <- dl %>% filter (time==56) %>% select(NUMERO, grep("Q", colnames(dl)))

# h0$global <- apply(h0[,-1],1,sum) 
# h56$global <- apply(h56[,-1],1,sum) 

#calcul des sous scores de scl90
sc0[,dimensions] <- sapply(dimensions,function(x){
  data <- sc0[,get(x)]
  res <- apply(data,1,function(i)sum(i,na.rm=F))#faire une moyenne ou une somme ne change rien
}) 

sc56[,dimensions] <- sapply(dimensions,function(x){
  data <- sc56[,get(x)]
  res <- apply(data,1,function(i)sum(i,na.rm=F))
}) 
#Rappel: pour scl90, Na imputés en médiane dans data_management.R lorsque au moins un item est renseigné 
#(c'est à dire lorsque la ligne était presente dans SCL90)




#créer un tableau avec les scores globaux uniquement (qu'il faut calculer) : 4 façons

# #façon 1 : utilisation du fichier long
# dwh <- unique(dw[,c("NUMERO","GROUPE")])
# for (i in  as.numeric(names(table(dl$time)))){
#   .c <- dl %>% filter(time==i) %>% select(NUMERO, grep("HAM",colnames(dl))) 
#   .c[ , paste0("tot_",i)] <- apply(.c[,-1],1,sum)
#   assign(paste0("h",i),.c)
#   dwh <- merge(dwh,.c[ , c("NUMERO",paste0("tot_",i))],by="NUMERO")
# }
# dwh1 <- dwh
# rownames(dwh1) <- 1:nrow(dwh1)
# 
# #façon 2 en faisant les merges 1 à 1 (chronophage mais plus sûr) (je réutilise les scores calcules dans la façon 1)
# dwh <- merge(h0[,c("NUMERO","tot_0")],h4[,c("NUMERO","tot_4")],by="NUMERO")
# dwh <- merge(dwh,h7[,c("NUMERO","tot_7")],by="NUMERO")
# dwh <- merge(dwh,h14[,c("NUMERO","tot_14")],by="NUMERO")
# dwh <- merge(dwh,h21[,c("NUMERO","tot_21")],by="NUMERO")
# dwh <- merge(dwh,h28[,c("NUMERO","tot_28")],by="NUMERO")
# dwh <- merge(dwh,h42[,c("NUMERO","tot_42")],by="NUMERO")
# dwh <- merge(dwh,h56[,c("NUMERO","tot_56")],by="NUMERO")
# dwh <- merge (dwh,gpe,by="NUMERO",all=T)
# dwh2 <- dwh[,c(1,10,2:9)]
# 
# #comparaison des façons 1 et 2
# all.equal(dwh1,dwh) #ok sauf groupe qui est facteur
# 
# #façon 3 en utilisant le format wide
# time <- as.numeric(names(table(dl$time))) #vecteur contenant tous les temps
# dwh <- dw[,!colnames(dw) %in% colnames(dw)[grep("Q",colnames(dw))]]#je supprime l'échelle scl pour aller plus vite (inutile ici mais utile si on fait du big data) 
# dwh[ ,paste0("tot_",time)] <- sapply(paste0("_",time), function(i) {
#   #res <- apply(dwh[,grep(i,colnames(dwh))],1,sum) #FAUX! attention : grep _4 prend aussi _42...=> utiliser str_sub
#   .col <- if (nchar(i)==2) str_sub(colnames(dwh),-2,-1)==i else str_sub(colnames(dwh),-3,-1)==i
#   .col <- colnames(dwh)[.col]
#   res <- apply(dwh[ ,.col],1,sum) 
#   return(res)
# })
# dwh <- dwh[,c("NUMERO","GROUPE",colnames(dwh)[grep("tot_",colnames(dwh))])]
# rownames(dwh) <- 1:nrow(dwh)
# dwh3 <- dwh
# 
# #comparaison des façons 1 et 3
# all.equal(dwh1,dwh3)#ok (qd on fixe les numéros de ligne de dwh1 qui étaient un peu étranges)
#saveRDS(dwh,"data/dwh.rds") #dwh pour data wide hamilton
#
# #façon 4 : je calcul en une étape dans le fichier long puis reshape
# dlh <- dl[,1:20]
# dlh$tot <- apply(dlh[,4:ncol(dlh)],1,sum)
# dlh <- dlh[,c("NUMERO","GROUPE","time","tot")]
# saveRDS (dlh,"data/dlh.rds")
# dwh4 <- reshape (dlh, direction="wide", timevar = "time", idvar = "NUMERO",   
#                  v.names="tot", sep="_") #nb : on peut choisir v.names pour direction long mais pour direction wide:on reprend le nom de la variable qui varie
# dwh4 <- data.frame(dwh4) #pour supprimer attributes "reshapeWide qui empeche la comparaison
# rownames(dwh4) <- 1:nrow(dwh4)
# all.equal(dwh4,dwh)#ok ( qd on supprime attributes et qu'on renomme les rangs)
