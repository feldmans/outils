library(psy)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(knitr)

source ("src/objects_outils.R") #Je charge les objets crées par le data management(évite de tout relancer)

summary(d)
head(dw)
head(dl)

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
dimensions <- c("somatisation","symptobs","sensitivite","depression","anxiete","hostilite","phobie","parano")
somatisation <- paste0("Q",c(1,4,12,27,42,48,49,52,53,56,58,40))
symptobs <- paste0("Q",c(9,10,28,38,3,4,46,51,55,65))
sensitivite <- paste0("Q",c(6,21,34,36,37,41,61,69,73))
depression <- paste0("Q",c(5,14,15,20,22,26,29,30,31,32,54,71,79))
anxiete <- paste0("Q",c(2,17,23,33,39,57,72,78,80,86))
hostilite <- paste0("Q",c(11,24,63,67,74,81))
phobie <- paste0("Q",c(13,25,47,70,75,82,50))
parano <- paste0("Q",c(8,18,43,68,76,83))

#Description des données manquantes pour sc0
table(apply(apply(sc0[,grep("Q",colnames(sc0))],2,is.na),1,sum))
#   0  90 
# 143   3 
#143 ont des données complètes, 3 ont une échelle non remplie (NA pour les 90 items)

#calcul du score de hamilton et des sous scores de scl90
dl0$global <- apply(dl0[,-1],1,sum) #de 0 à 52
sc0[,dimensions] <- sapply(dimensions,function(x){
  data <- sc0[,get(x)]
  res <- apply(data,1,function(i)sum(i,na.rm=F))#faire une moyenne ou une somme ne change rien
}) #Rappel: Na imputés en médiane lorsque au moins un item est renseigné (c'est à dire lorsque la ligne était presente dans SCL90)

#corrélation:
cor(dl0$global,sc0[ ,dimensions],use="complete.obs") #j'ai bien fait attention à ce que les numero de dl0 et sc0 soit dans le meme ordre
#le score de hamilton corrèle le plus avec dimension 1(somatisation), 2(symptomes obsessionels), 4(dépression)

#ACP focalisée: 
dlsc0 <- merge(dl0[,c("NUMERO","global")], sc0[ ,c("NUMERO",dimensions)],by="NUMERO",all.x=T,all.y=F)
fpca(y="global",x=dimensions,data=dlsc0)



#valider hamilton au temps t=56
dl56 <- dl %>% filter(time==56) %>% select(NUMERO, grep("HAM",colnames(dl)))
sc56 <- dl %>% filter (time==56) %>% select(NUMERO, grep("Q", colnames(dl)))

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
mat <- cor(dl56[,-1])

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
print(fit.1,digits=2, cutoff=.1,sort=F) #fait une rotation varimax automatiquement

#A faire si temps ACP pour variables et sujets (et se demander si normaliser ou non)


#C- cohérence interne
set.seed(1234) #pour avoir un résultat reproductible
BootCronCi(dl56,100)

#D- validité concourrante et divergente
#Description des données manquantes pour sc56
table(apply(apply(sc56[,grep("Q",colnames(sc56))],2,is.na),1,sum))
# 0  1   2  4  7 90 
# 94 18  3  1  1 29  
#94 ont des données complètes, 3 ont une échelle non remplie (NA pour les 90 items)

#calcul du score de hamilton et des sous scores de scl90
dl56$global <- apply(dl56[,-1],1,sum) #de 0 à 52
sc56[,dimensions] <- sapply(dimensions,function(x){
  data <- sc56[,get(x)]
  res <- apply(data,1,function(i)sum(i,na.rm=F))
}) #Rappel: Na imputés en médiane lorsque au moins un item est renseigné (c'est à dire lorsque la ligne était presente dans SCL90)

#corrélation:
cor(dl56$global,sc56[ ,dimensions],use="complete.obs") #j'ai bien fait attention à ce que les numero de dl56 et sc56 soit dans le meme ordre
#le score de hamilton corrèle le plus avec dépression

#ACP focalisée: 
dlsc56 <- merge(dl56[,c("NUMERO","global")], sc56[ ,c("NUMERO",dimensions)],by="NUMERO",all.x=T,all.y=F)
fpca(y="global",x=dimensions,data=dlsc56)
