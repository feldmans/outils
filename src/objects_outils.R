library(psy)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(knitr)
library(stringr)
library(survival)
library(boot)
library(ReporteRs)

#gpe <- read.csv2("data/groupe.csv") #utilisé dans calcul des scores globaux façon 2
dw <- readRDS("data/dw.rds")
dl <- readRDS("data/dl.rds")
dwh <- readRDS("data/dwh.rds") #ne contient que les scores globaux de hamilton aux différents temps (utile pour Q2 LOCF et Q3)
dlh <- readRDS("data/dlh.rds") #contient les scores globaux, les temps NUMERO et groupe en format long
h0 <- dl %>% filter(time==0) %>% select(NUMERO, grep("HAM",colnames(dl)))
sc0 <- dl %>% filter (time==0) %>% select(NUMERO, grep("Q", colnames(dl)))
h56 <- dl %>% filter(time==56) %>% select(NUMERO, grep("HAM",colnames(dl)))
sc56 <- dl %>% filter (time==56) %>% select(NUMERO, grep("Q", colnames(dl)))
h0$global <- apply(h0[,-1],1,sum) 
h56$global <- apply(h56[,-1],1,sum) 



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
