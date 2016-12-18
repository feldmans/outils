library(stringr)
library(dplyr)

gpe <- read.csv2("data/groupe.csv")
hdrs <- read.csv2("data/hdrs.csv")
scl <- read.csv2("data/SCL90.csv")

gpe$NUMERO <- as.integer(gpe$NUMERO)
gpe$GROUPE <- as.factor(gpe$GROUPE)


#Je transforme les colonnes en variables quantitatives (et NUMERO en chiffre pour ne pas avoir de problemes dus aux facteurs)

summary(hdrs)
 hdrs$NUMERO <- as.integer(hdrs$NUMERO)
# for (i in colnames(hdrs)[grep("HAM",colnames(hdrs))]){
#   hdrs[,i] <- as.factor(hdrs[ ,i]) 
#}
for (i in colnames(hdrs)[grep("HAM",colnames(hdrs))]){
  hdrs[,i] <- as.numeric(as.character(hdrs[ ,i])) #sinon prend le rang du facteur
}

summary(scl)
scl$NUMERO <- as.integer(scl$NUMERO)
# for (i in colnames(scl)[grep("Q",colnames(scl))]){
#   scl[,i] <- as.factor(scl[ ,i]) 
# }
for (i in colnames(scl)[grep("Q",colnames(scl))]){
  scl[,i] <- as.numeric(as.character(scl[ ,i])) #"NAs introduce by coercion" car il ya des ND
}

d <- merge(gpe, hdrs, by="NUMERO",all.x=T)
d <- merge(d, scl, by=c("NUMERO","VISIT"),all.x=T,all.y=T)

#ATTENTION : en lisant l'échelle de hamilton, je vois que les questions 16A et 16B
#ne peuvent être renseignés en même temps, renseigne la même info et sont codés de la même manière

#en effet aucune lignes renseignés pour les 2
d[!is.na(d$HAMD16A) & !is.na(d$HAMD16B),]
d[is.na(d$HAMD16A) & is.na(d$HAMD16B),] #une seule ligne est NA pour les 2 : 128

d$HAMD16 <- ifelse (!is.na(d$HAMD16A), d$HAMD16A, d$HAMD16B)
d$HAMD16A <- NULL
d$HAMD16B <- NULL
d <- d[ , c( (1:grep("HAMD15",colnames(d))),grep("HAMD16",colnames(d)),(grep("HAMD17",colnames(d)) : (ncol(d)-1)))]

#verif que chaque sujet a bien un seul groupe (verif que merge a bien marché)
a <- table(d$NUMERO,d$GROUPE)
table(a[,2] != 0 & a[,1]!=0) #personne n'a 2 groupe

d$time <- as.integer(str_sub(d$VISIT,2,-1))
d$VISIT <- NULL
d <- d[, c(1,2,grep("time",colnames(d)),3: (ncol(d)-1))]
d <- d[order(d$time,d$NUMERO),]
#C'est un fichier long : 1 ligne par consultation  
#reshape
#dr <- reshape(d, direction="wide", timevar = c("J0",  "J14", "J21", "J28", "J4",  "J42", "J56", "J7"), idvar = "NUMERO", v.names=c(colnames(d)[grep("HAM",colnames(d))],colnames(d)[grep("Q",colnames(d))]))

#wide : 1 ligne par patient avec les questionnaire aux différents temps sur la même ligne
dw <- reshape (d, direction="wide", times = c(0, 4, 7, 14, 21, 28, 42, 56), idvar = "NUMERO", 
               v.names=c(colnames(d)[grep("HAM",colnames(d))],colnames(d)[grep("Q",colnames(d))]), sep="_")
#long : 1 ligne par consultation (d est déjà en format long mais pour l'exercice, je le refait à partir de dw)
# dl <- reshape (dw, direction="long",varying=c(grep("HAM",colnames(dw)),grep("Q",colnames(dw))),times =  c(0, 4, 7, 14, 21, 28, 42, 56),
#                idvar = "NUMERO", v.names=c(colnames(d)[grep("HAM",colnames(d))],colnames(d)[grep("Q",colnames(d))]),sep="_" )
dl <- reshape (dw, direction="long",varying=c(grep("HAM",colnames(dw)),grep("Q",colnames(dw))),
               idvar = "NUMERO", sep="_")


#comparaison
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



#Pour connaître le nombre de visites manquantes, je préfère pour l'instant garder le tableau 
saveRDS(dw,"data/dw.rds")
saveRDS(dl,"data/dl.rds")



#A faire plus tard pour prendre les noms de colonne de dw plutôt que de d (mais pb il faut changer les . en _)
a <- "HAMD1.0" 
a<- "HAMD12.50"
is.na(as.integer(str_sub(a,-3,-3)))

a<- "HAMD12_50"
a <- "HAMD1_0" 
grep("_",str_sub(a,-3,-3)) #bizarre le pattern "." est reconnu dans 1
#faire un ifelse grep existe, str_sub(a,-1,-2), str_sub(a,-1,-3)



is.integer(NA)
