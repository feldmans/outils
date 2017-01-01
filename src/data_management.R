#####################
# data_management.R #
#####################


#Je charge les librairies utilisées 
library(stringr)
library(dplyr)

#J'importe les 3 fichiers
gpe <- read.csv2("data/groupe.csv")
hdrs <- read.csv2("data/hdrs.csv")
scl <- read.csv2("data/SCL90.csv")


#PREPARATION DES BASES "groupe", "hdrs", ET "scl90" ET MERGE

#NUMERO et GROUPE en numérique 
gpe$NUMERO <- as.integer(gpe$NUMERO)
gpe$GROUPE <- as.factor(gpe$GROUPE)
hdrs$NUMERO <- as.integer(hdrs$NUMERO) #pour ne pas avoir des probleme lors des merges
scl$NUMERO <- as.integer(scl$NUMERO) #pour ne pas avoir des probleme lors des merges


#recherche de valeurs aberrantes
summary(hdrs) #pas de valeurs aberrantes
summary(scl) #beaucoup de valeurs aberrantes (superieures à 4)

 
#HAMILTON : uniformisation des colonnes en numerique 
for (i in colnames(hdrs)[grep("HAM",colnames(hdrs))]){
  hdrs[,i] <- as.numeric(as.character(hdrs[ ,i])) #sinon prend le rang du facteur
}

#SCL90 : uniformisation des colonnes en numerique et gestion des données aberrantes
for (i in colnames(scl)[grep("Q",colnames(scl))]){
  scl[,i] <- as.numeric(as.character(scl[ ,i])) #"NAs introduce by coercion" car il ya des ND et des ""
  #Je change les valeurs aberrantes (superieures à 4) en NA
  scl[,i] <- ifelse (scl[,i] > 4, NA, scl[,i]) 
  #J'impute les NA en valeur mediane (ce n'est utilisé que pour des calculs exploratoires donc sans conséquence sur la conclusion)
  scl[,i] <- ifelse (is.na(scl[,i]), median(scl[,i],na.rm=T), scl[,i]) 
}

#merge des 3 bases
d <- merge(gpe, hdrs, by="NUMERO",all.x=T)
d <- merge(d, scl, by=c("NUMERO","VISIT"),all.x=T,all.y=T)

#Fusion des questions 16A et 16B de l'echelle de Hamilton
d$HAMD16 <- ifelse (!is.na(d$HAMD16A), d$HAMD16A, d$HAMD16B)
d$HAMD16A <- NULL
d$HAMD16B <- NULL
d <- d[ , c( (1:grep("HAMD15",colnames(d))),grep("HAMD16",colnames(d)),(grep("HAMD17",colnames(d)) : (ncol(d)-1)))]


#Je recode VISIT en time(ça me parle plus), et je le met en numérique 
#(à refaire, je garderai le nom VISITE pour plus de clareté pour les correcteurs)
d$time <- as.integer(str_sub(d$VISIT,2,-1))
d$VISIT <- NULL
d <- d[, c(1,2,grep("time",colnames(d)),3: (ncol(d)-1))] #Je change l'ordre des colonnes, dabord numéro et time puis le reste
d <- d[order(d$time,d$NUMERO),] #Je réordonne mes lignes : selon time puis numéro
#C'est un fichier long : 1 ligne par consultation  




#PASSAGE DE LONG EN WIDE ET DE WIDE EN LONG AVEC RESHAPE

#wide : 1 ligne par patient avec les questionnaire aux différents temps sur la même ligne
dw <- reshape (d, direction="wide", timevar = "time", idvar = "NUMERO", 
               v.names=c(colnames(d)[grep("HAM",colnames(d))],colnames(d)[grep("Q",colnames(d))]), sep="_")
#long : 1 ligne par consultation (d est déjà en format long mais pour l'exercice, je le refait à partir de dw)
dl <- reshape (dw, direction="long",idvar = "NUMERO",
               varying=c(grep("HAM",colnames(dw)),grep("Q",colnames(dw))), sep="_")

#VERIFICATIONS:
#comparaisons du fichier mergé de base et de celui obtenu par wide puis long
all.equal(d,dl) #beaucoup de différence, notamment parce que la longueur des tableau diffère

#suppression des lignes all NA 
dlnoNA <- dl[-which(apply(dl[,c(4:ncol(dl))],1,function(x)sum(is.na(x)))==(ncol(dl)-3)), ]
dlnoNA <- dlnoNA[order(dlnoNA$NUMERO,dlnoNA$time),]
dnoNA <- d[-which(apply(d[,c(4:ncol(d))],1,function(x)sum(is.na(x)))==(ncol(d)-3)), ]
dnoNA <- dnoNA[order(dnoNA$NUMERO,dnoNA$time),]
#renumérotation des lignes
rownames(dnoNA) <- 1:nrow(dnoNA)
rownames(dlnoNA) <- 1:nrow(dlnoNA)
#comapraison à nouveau
all.equal(dlnoNA[,1],dnoNA[,1])#Les patients sont bien les memes dans le meme ordre
all.equal(dlnoNA[,-1],dnoNA[,-1]) #Je retire cette première colonne pour vérifier le reste, permet de retirer la section "attributes" liées à reshape)
#TRUE! les tableaux sont identiques

#Je sauvegarde les tableau long (dl) et wide (dw) dans le dossier data
saveRDS(dw,"data/dw.rds")
saveRDS(dl,"data/dl.rds")




#SELECTION DES ECHELLES HAMILTON ET SCL90 à J0 et J56 

#Dataset hamilton aux temps 0 et 56 :
dl0 <- dl %>% filter(time==0) %>% select(NUMERO, grep("HAM",colnames(dl)))
dl56 <- dl %>% filter(time==56) %>% select(NUMERO, grep("HAM",colnames(dl)))

#Dataset scl90 aux temps 0 et 56 :
sc0 <- dl %>% filter (time==0) %>% select(NUMERO, grep("Q", colnames(dl)))
sc56 <- dl %>% filter (time==56) %>% select(NUMERO, grep("Q", colnames(dl)))

#calcul des sous scores de scl90
sc0[,dimensions] <- sapply(dimensions,function(x){
  data <- sc0[,get(x)]
  res <- apply(data,1,function(i)sum(i,na.rm=F))#faire une moyenne ou une somme ne change rien
}) 

sc56[,dimensions] <- sapply(dimensions,function(x){
  data <- sc56[,get(x)]
  res <- apply(data,1,function(i)sum(i,na.rm=F))
}) 
#Rappel: pour scl90, Na imputés en médiane lorsque au moins un item est renseigné 
#(c'est à dire lorsque la ligne était presente dans SCL90)

#Les scores pour dl0 et dl56 seront calculés dans l'analyse, car j'ai besoin des tableaux sans le score au départ

#Je sauvegarde dans data
saveRDS(dl0, "data/dl0.rds")
saveRDS(sc0, "data/sc0.rds")
saveRDS(dl56, "data/dl56.rds")
saveRDS(sc56, "data/sc56.rds")




#CREATION D'UN TABLEAU dwh (d wide hamilton) AVEC LES SCORES de HAMILTON AUX DIFFERENTS TEMPS

# 4 façons:

#façon 1 : utilisation du fichier long
dwh <- unique(dw[,c("NUMERO","GROUPE")])
for (i in  as.numeric(names(table(dl$time)))){
  .c <- dl %>% filter(time==i) %>% select(NUMERO, grep("HAM",colnames(dl)))
  .c[ , paste0("tot_",i)] <- apply(.c[,-1],1,sum)
  assign(paste0("h",i),.c)
  dwh <- merge(dwh,.c[ , c("NUMERO",paste0("tot_",i))],by="NUMERO")
}
dwh1 <- dwh
rownames(dwh1) <- 1:nrow(dwh1)

#façon 2 en faisant les merges 1 à 1 (chronophage mais plus sûr) (je réutilise les scores calcules dans la façon 1)
dwh <- merge(h0[,c("NUMERO","tot_0")],h4[,c("NUMERO","tot_4")],by="NUMERO")
dwh <- merge(dwh,h7[,c("NUMERO","tot_7")],by="NUMERO")
dwh <- merge(dwh,h14[,c("NUMERO","tot_14")],by="NUMERO")
dwh <- merge(dwh,h21[,c("NUMERO","tot_21")],by="NUMERO")
dwh <- merge(dwh,h28[,c("NUMERO","tot_28")],by="NUMERO")
dwh <- merge(dwh,h42[,c("NUMERO","tot_42")],by="NUMERO")
dwh <- merge(dwh,h56[,c("NUMERO","tot_56")],by="NUMERO")
dwh <- merge (dwh,gpe,by="NUMERO",all=T)
dwh2 <- dwh[,c(1,10,2:9)]

#façon 3 en utilisant le format wide
time <- as.numeric(names(table(dl$time))) #vecteur contenant tous les temps
dwh <- dw[,!colnames(dw) %in% colnames(dw)[grep("Q",colnames(dw))]]#je supprime l'échelle scl pour aller plus vite (inutile ici mais utile si on fait du big data)
dwh[ ,paste0("tot_",time)] <- sapply(paste0("_",time), function(i) {
  #res <- apply(dwh[,grep(i,colnames(dwh))],1,sum) #FAUX! attention : grep _4 prend aussi _42...=> utiliser str_sub
  .col <- if (nchar(i)==2) str_sub(colnames(dwh),-2,-1)==i else str_sub(colnames(dwh),-3,-1)==i
  .col <- colnames(dwh)[.col]
  res <- apply(dwh[ ,.col],1,sum)
  return(res)
})
dwh <- dwh[,c("NUMERO","GROUPE",colnames(dwh)[grep("tot_",colnames(dwh))])]
rownames(dwh) <- 1:nrow(dwh)
dwh3 <- dwh

#façon 4 : je calcul en une étape dans le fichier long puis reshape
dlh <- dl[,1:20]
dlh$tot <- apply(dlh[,4:ncol(dlh)],1,sum)
dlh <- dlh[,c("NUMERO","GROUPE","time","tot")]
saveRDS (dlh,"data/dlh.rds")
dwh4 <- reshape (dlh, direction="wide", timevar = "time", idvar = "NUMERO",
                 v.names="tot", sep="_") #nb : on peut choisir v.names pour direction long mais pour direction wide:on reprend le nom de la variable qui varie
dwh4 <- data.frame(dwh4) #pour supprimer attributes "reshapeWide qui empeche la comparaison
rownames(dwh4) <- 1:nrow(dwh4)

saveRDS(dwh3,"data/dwh.rds") #dwh pour data wide hamilton



#CREATION D'UN TABLEAU dws (data, wide, survie) AVEC LE DELAI JUSQU'A LA DIMINUTION D'AU MOINS 50% DU SCORE DE HAMILTON

#Je pars du tableau dwh qui contient les scores de hamilton à tous les temps. 
dws <- dwh

#Création de la variable date_evt qui indique à quelle date l'évènement survient (s'il survient, sinon NA)
date_evt <- lapply(1:nrow(dws), function(i){
  #browser()
  .l <- as.vector(dws[i, grep("tot_",colnames(dws))])
  .l <- .l <= (dws[i, "tot_0"]/2) #version 50% inclu
  #.l <- .l < (dws[i, "tot_0"]/2) #version 50% exclu
  prem_chute <- colnames(dws[,3:ncol(dws)])[head(which(.l==TRUE),1)]
  if (length(prem_chute)==0) res <- NA
  else {
    if (str_sub(prem_chute,-3,-3)=="_") res <- str_sub(prem_chute,-2,-1)
    else res <- str_sub(prem_chute, -1,-1)
  }
  return (res)
})
dws$date_evt <- as.numeric(do.call(rbind,date_evt))


#Création de la variable evt : s'il y a une date d'évènement, alors le patient est evt=1
dws$evt <- ifelse(!is.na(dws$date_evt),1,0)


#création de la vairable ddn pour date des dernières nouvelles

#C'est à dire dernier questionnaire disponible si pas d'evenement ou date de l'evement si evt=1.
#Pour des raisons pratiques, je cherche la date des dernières nouvelles pour tout le monde
#que je modifie pour la date de l'évènement dans le cas echeant.

ddn <- lapply(1:nrow(dws), function(i){
  #browser()
  .l <- as.vector(dwh[i, 3:ncol(dwh)])
  .l <- !is.na(.l)
  ddn <- colnames(.l)[tail(which(.l==TRUE),1)] #dernier questionnaire renseigné
  if (length(ddn)==0) res <- NA
  else {
    if (str_sub(ddn,-3,-3)=="_") res <- str_sub(ddn,-2,-1)
    else res <- str_sub(ddn, -1,-1)
  }
  return (res)
})
dws$ddn <- as.numeric(do.call(rbind,ddn))

#Si il y a eu un evenement alors la ddn est la date de l'evement
dws$ddn <- ifelse (dws$evt==1, dws$date_evt, dws$ddn)

#ici je n'ai pas des dates mais le nombre de jour après le premier questionnaire, donc
#c'est deja des duree de suivie
dws$time <- dws$ddn

#pour une analyse de survie, je n'ai besoin que de l'evt, de la duree de suivi,
#et bien sûr de l'identifiant du patient et de son groupe
dws <- dws[,c("NUMERO","GROUPE","evt","time")]

#Je sauvegarde
saveRDS (dws, "data/dws.rds")
