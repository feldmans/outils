################
#  QUESION 1   #
################

source ("src/objects_outils.R") #Je charge les objets crées par le data management(évite de tout relancer)

summary(d)
head(dw)
head(dl)

#description de la basehdrs:

#nb de visites
a <- dwh
b <- apply(dwh[,grep("tot",colnames(dwh))],2,function(x)!is.na(x))
a$nvisites <- apply(b,1,sum)
a %>% filter (nvisites!=8 & !is.na(tot_56))
qplot(as.factor(a[,"nvisites"]),xlab="Nombre de visites effectuées", ylab="Nombre de patients", fill=I("navajowhite3"), col=I("pink4"))
#nb de patients à J56
dl %>% filter(time==56 & !is.na(HAMD1)) %>% count



#1/valider hamilton au temps t=0

dl0 <- dl %>% filter(time==0) %>% select(NUMERO, grep("HAM",colnames(dl)))
sc0 <- dl %>% filter (time==0) %>% select(NUMERO, grep("Q", colnames(dl)))

summary(dl0)
summary(sc0)

#A- analyse d'items: 

#A1-diagramme en baton
pl <- lapply(colnames(dl0)[-1], function(.x) qplot(as.factor(dl0[,.x]),xlab=NULL, main=paste("plot", .x),fill=I("navajowhite3"), col=I("pink4")))
ml <- marrangeGrob(pl,ncol=4,nrow=5,top = NULL)
print(ml)

#A2- nb de NA
#qplot aurait affiché les NA s'il y en avait, mais 2 vérifications vallent mieux qu'une
table(apply(apply(dl0[,grep("HAM",colnames(dl0))],2,is.na),1,sum))

#A3- corrélation entre items
mat <- cor(dl0[,-1])

couples<-lapply(c(0.2,0.4),function(w){
  #pour supprimer les doublons
  mat2<- lower.tri(mat,diag=FALSE)
  rownames(mat2)<-rownames(mat)
  colnames(mat2) <- colnames(mat) 
  mat2 <- ifelse(mat2==TRUE,mat,0) 
  #pour chercher les coefficients de corrélation superieur à w
  w_r <- which(abs(mat2)>=w )
  #pour trouver les noms de ligne et colonne de ces coefficients
  which_couple <- lapply(w_r,function(x){
    k <- arrayInd(x, dim(mat2))
    d<-data.frame(var1=rownames(mat2)[k[,1]], var2=colnames(mat2)[k[,2]],r=mat2[x])
    return(d)
  })
  #Je colle les listes
  which_couple <- data.frame(do.call(rbind,which_couple))
  return(which_couple)
})
couples_rename <- couples
colnames(couples_rename[[1]])<- c("variable 1","variable 2", "coefficient de corrélation")

couplesup0.2 <- couples_rename[[1]]#la liste 1 a toutes les corrélations supérieures à 0.2, le 2 a seulement celles sup à 0.4
kable(couplesup0.2[order(abs(couplesup0.2[3])), ]) 

#B- structure dimensionnelle
#B1- diagramme des valeurs propres
scree.plot(dl0[,-1], simu=20)
scree.plot(dl0[,-1], type="R", simu=20)

#B2-Analyse factorielle
fit.3 <- factanal(dl0[, -1], factors = 3, rotation = "varimax")
print(fit.3, digits=2, cutoff=.1,sort=F)
fit.2 <- factanal(dl0[, -1], factors = 2, rotation = "varimax")
print(fit.2,digits=2, cutoff=.1,sort=F) 
fit.1 <-factanal(dl0[, -1], factors = 1, rotation = "varimax")
print(fit.1,digits=2, cutoff=.1,sort=F) #fait une rotation varimax automatiquement



#A faire si temps ACP pour variables et sujets 


#C- cohérence interne
#Je ne fais qu'un seul cronbach car le score est calculé par la somme de tous les items
set.seed(123) #pour avoir un résultat reproductible
BootCronCi(dl0,100)

#D- validité concourrante et divergente

#Description des données manquantes pour sc0

sclbis <- read.csv2("data/SCL90.csv")
for (i in colnames(sclbis)[grep("Q",colnames(sclbis))]){
  sclbis[,i] <- as.numeric(as.character(sclbis[ ,i])) #"NAs introduce by coercion" car il ya des ND et des ""
  #Je change les valeurs aberrantes (superieures à 4) en NA
  sclbis[,i] <- ifelse (sclbis[,i] > 4, NA, sclbis[,i])
}
sclbis <- sclbis %>% filter (VISIT=="J0")
table(apply(sclbis[ ,grep("Q",colnames(sclbis))],1,sum),useNA = "a") #112 questionnaires complets
table(apply(apply(sclbis[ ,grep("Q",colnames(sclbis))],2,is.na),1,sum),useNA = "a") #moins de 12 items manquants par patient


table(apply(apply(sc0[,grep("Q",colnames(sc0))],2,is.na),1,sum))
#   0  90 
# 143   3 
#143 ont des données complètes, 3 ont une échelle non remplie (NA pour les 90 items)

#calcul du score global de Hamilton
dl0$global <- apply(dl0[,-1],1,sum) #de 0 à 52

#corrélation:
cor(dl0$global,sc0[ ,dimensions],use="complete.obs") #j'ai bien fait attention à ce que les numero de dl0 et sc0 soit dans le meme ordre
#le score de hamilton corrèle le plus avec dimension 1(somatisation), 2(symptomes obsessionels), 4(dépression)

#ACP focalisée: 
dlsc0 <- merge(dl0[,c("NUMERO","global")], sc0[ ,c("NUMERO",dimensions)],by="NUMERO",all.x=T,all.y=F)
fpca(y="global",x=dimensions,data=dlsc0)



#valider hamilton au temps t=56


summary(dl56)
summary(sc56)

#A- analyse d'items: 

#A1-diagramme en baton
pl <- lapply(colnames(dl56)[-1], function(.x) qplot(as.factor(dl56[,.x]),xlab=NULL, main=paste("plot", .x),fill=I("navajowhite3"), col=I("pink4")))
ml <- marrangeGrob(pl,ncol=2,nrow=3,top = NULL)
print(ml)

#A2- distribution des NA selon les sujets
table(apply(apply(dl56[,grep("HAM",colnames(dl56))],2,is.na),1,sum))
#120 patients ont une échelle complète, 26 patients ont une échelle non remplie (NA aux 17 items)

#A3- corrélation entre items
mat <- cor(dl56[,-1],use="complete.obs") #attention il y a des NA! la matrice est NA si on ne les supprime pas 

couples<-lapply(c(0.2,0.4),function(w){
  #pour supprimer les doublons
  mat2<- lower.tri(mat,diag=FALSE)
  rownames(mat2)<-rownames(mat)
  colnames(mat2) <- colnames(mat) 
  mat2 <- ifelse(mat2==TRUE,mat,0) 
  #pour chercher les coefficients de corrélation superieur à w
  w_r <- which(abs(mat2)>=w )
  #pour trouver les noms de ligne et colonne de ces coefficients
  which_couple <- lapply(w_r,function(x){
    k <- arrayInd(x, dim(mat2))
    d<-data.frame(var1=rownames(mat2)[k[,1]], var2=colnames(mat2)[k[,2]],r=mat2[x])
    return(d)
  })
  #Je colle les listes
  which_couple <- data.frame(do.call(rbind,which_couple))
  return(which_couple)
})
couples_rename <- couples
colnames(couples_rename[[1]])<- c("variable 1","variable 2", "coefficient de corrélation")

couplesup0.2 <- couples_rename[[1]]#la liste 1 a toutes les corrélations supérieures à 0.2, le 2 a seulement celles sup à 0.4
kable(couplesup0.2[order(abs(couplesup0.2[3])), ]) 

#B- structure dimensionnelle
#B1- diagramme des valeurs propres
scree.plot(dl56[,-1], simu=20)
scree.plot(dl56[,-1], type="R", simu=20)

#B2-Analyse factorielle
fit.3 <- factanal(dl56[!is.na(dl56$HAMD1), -1], factors = 3, rotation = "varimax") #retire les NA, si 1 item NA , tous NA comme vu plus haut
print(fit.3, digits=2, cutoff=.1,sort=F)
fit.2 <- factanal(dl56[!is.na(dl56$HAMD1), -1], factors = 2, rotation = "varimax")
print(fit.2,digits=2, cutoff=.1,sort=F) 
fit.1 <-factanal(dl56[!is.na(dl56$HAMD1), -1], factors = 1, rotation = "varimax")
print(fit.1,digits=2, cutoff=.1,sort=F) 

#A faire si temps ACP pour variables et sujets (et se demander si normaliser ou non)


#C- cohérence interne
set.seed(1234) #pour avoir un résultat reproductible
BootCronCi(dl56,100)

#D- validité concourrante et divergente

#calcul du score global de Hamilton
dl56$global <- apply(dl56[,-1],1,sum) #de 0 à 52

#Description des données manquantes pour sc56
sclbis <- read.csv2("data/SCL90.csv")
for (i in colnames(sclbis)[grep("Q",colnames(sclbis))]){
  sclbis[,i] <- as.numeric(as.character(sclbis[ ,i])) #"NAs introduce by coercion" car il ya des ND et des ""
  #Je change les valeurs aberrantes (superieures à 4) en NA
  sclbis[,i] <- ifelse (sclbis[,i] > 4, NA, sclbis[,i])
}
sclbis <- sclbis %>% filter (VISIT=="J56")
table(apply(sclbis[ ,grep("Q",colnames(sclbis))],1,sum),useNA = "a") #112 questionnaires complets
table(apply(apply(sclbis[ ,grep("Q",colnames(sclbis))],2,is.na),1,sum),useNA = "a") #moins de 12 items manquants par patient

#corrélation:
cor(dl56$global,sc56[ ,dimensions],use="complete.obs") #j'ai bien fait attention à ce que les numero de dl56 et sc56 soit dans le meme ordre
#le score de hamilton corrèle le plus avec dépression

#ACP focalisée: 
dlsc56 <- merge(dl56[,c("NUMERO","global")], sc56[ ,c("NUMERO",dimensions)],by="NUMERO",all.x=T,all.y=F)
fpca(y="global",x=dimensions,data=dlsc56)
