library(stringr)
library(dplyr)

gpe <- read.csv2("data/groupe.csv")
hdrs <- read.csv2("data/hdrs.csv")
scl <- read.csv2("data/SCL90.csv")



#Changement de classe et gestion des données manquantes/aberrantes

gpe$NUMERO <- as.integer(gpe$NUMERO)
gpe$GROUPE <- as.factor(gpe$GROUPE)

summary(hdrs) #pas de valeurs aberrantes
 hdrs$NUMERO <- as.integer(hdrs$NUMERO) #pour ne pas avoir des probleme lors des merges
#uniformisation des colonnes en numerique
for (i in colnames(hdrs)[grep("HAM",colnames(hdrs))]){
  hdrs[,i] <- as.numeric(as.character(hdrs[ ,i])) #sinon prend le rang du facteur
}

summary(scl) #beaucoup de valeurs aberrantes (superieures à 4)
scl$NUMERO <- as.integer(scl$NUMERO) #pour ne pas avoir des probleme lors des merges
#uniformisation des colonnes en numerique et gestion des données aberrantes
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

#NB : 3 patients n'ont pas de score scl90 du tout, je ne les impute pas
table(unique(hdrs$NUMERO) %in% unique(scl$NUMERO))
#1 patient n'a pas de score de hamilton à 1 date 
#(il y en aura plus quand on aura fait reshape, mais ce patient là était noté dans hdrs et la ligne était NA)
d%>% filter(is.na(HAMD1))#c'est le patient 128 à J7. NB ce patient n'était pas dans scl donc son score n'a pas été imputé par la médiane (heureusement; on reste coherent)

#Fusion des questions 16A et 16B de l'echelle de Hamilton

#en lisant l'échelle de hamilton, je vois que les questions 16A et 16B ne peuvent être renseignés
#en même temps, renseigne la même info et sont codés de la même manière

#aucune ligne renseignée pour les 2
d[!is.na(d$HAMD16A) & !is.na(d$HAMD16B),]
d[is.na(d$HAMD16A) & is.na(d$HAMD16B),] #une seule ligne est NA pour les 2 : 128

#fusion en HAMD16:
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




#Passage de long en wide et de wide en long avec reshape

#wide : 1 ligne par patient avec les questionnaire aux différents temps sur la même ligne
dw <- reshape (d, direction="wide", timevar = "time", idvar = "NUMERO", 
               v.names=c(colnames(d)[grep("HAM",colnames(d))],colnames(d)[grep("Q",colnames(d))]), sep="_")
#long : 1 ligne par consultation (d est déjà en format long mais pour l'exercice, je le refait à partir de dw)
# dl <- reshape (dw, direction="long",varying=c(grep("HAM",colnames(dw)),grep("Q",colnames(dw))),times =  c(0, 4, 7, 14, 21, 28, 42, 56),
#                idvar = "NUMERO", v.names=c(colnames(d)[grep("HAM",colnames(d))],colnames(d)[grep("Q",colnames(d))]),sep="_" )
dl <- reshape (dw, direction="long",varying=c(grep("HAM",colnames(dw)),grep("Q",colnames(dw))),
               idvar = "NUMERO", sep="_")

#------
#comparaisons du fichier mergé de base et de celui obtenu par wide puis long
all.equal(d,dl) #beaucoup de différence, notamment parce que la longueur des tableau diffère

#quand il n'y a pas de visite à un temps, reshape rajoute NA
table(dl$time)
table(d$time)
#par exemple au temps 4, individu 0 et d'autres n'ont pas de réponses et sont dans dl avec réponse NA mais pas dans d
dl %>% filter(is.na(dl$HAMD1) & dl$time==4) 
d %>% filter(NUMERO==0 & d$time==4) #cette ligne n'existe pas dans d et est complètement NA dans dl

#lignes all NA dans d(1 patient) et dans dl(116 patients)
table(apply(d[,c(4:ncol(d))],1,function(x)sum(is.na(x))))
table(apply(dl[,c(4:ncol(dl))],1,function(x)sum(is.na(x))))
#Liste des patients pour lesquels toutes les question (ncol(d)-3 == 107) sont NA
dl[which(apply(dl[,c(4:ncol(dl))],1,function(x)sum(is.na(x)))==107),c("NUMERO","time")]
d[which(apply(d[,c(4:ncol(d))],1,function(x)sum(is.na(x)))==107),c("NUMERO","time")]

#suppression des lignes all NA dans d et dl
dlnoNA <- dl[-which(apply(dl[,c(4:ncol(dl))],1,function(x)sum(is.na(x)))==(ncol(dl)-3)), ]
dlnoNA <- dlnoNA[order(dlnoNA$NUMERO,dlnoNA$time),]
dnoNA <- d[-which(apply(d[,c(4:ncol(d))],1,function(x)sum(is.na(x)))==(ncol(d)-3)), ]
dnoNA <- dnoNA[order(dnoNA$NUMERO,dnoNA$time),]

#la seule différence est rownames et attributes
all.equal(dlnoNA[,-c(1,3)],dnoNA[,-c(1,3)])
rownames(dnoNA) <- 1:nrow(dnoNA)
rownames(dlnoNA) <- 1:nrow(dlnoNA)
all.equal(dlnoNA[,-c(1,3)],dnoNA[,-c(1,3)])
# all.equal(dlnoNA,dnoNA)
# all.equal(dlnoNA[,-1],dnoNA[,-1]) #TRUE!
# identical(dlnoNA[,1],dnoNA[,1]) #TRUE!
# attributes(dnoNA) 
# attributes(dlnoNA) #comprend les info ajoutées par reshape

#fin de la comparaison
#---------


#Je garde les scores en NA pour les visites manquantes
saveRDS(dw,"data/dw.rds")
saveRDS(dl,"data/dl.rds")

