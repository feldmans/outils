library(psy)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(knitr)

dw <- readRDS("data/dw.rds")
dl <- readRDS("data/dl.rds")

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
ml <- marrangeGrob(pl,ncol=2,nrow=3,top = NULL)
print(ml)

#A2- nb de NA
#qplot aurait affiché les NA s'il y en avait, mais 2 vérifications vallent mieux qu'une
#qplot(as.factor(c(1,1,2,2,2,3,3,1,NA)))#qplot représente automatiquement les NA
apply(dl0,2,function(x)sum(is.na(x))) #aucun NA

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
print(factanal(dl0[, -1], factors = 3, rotation = "varimax"),cutoff = 0) #fait une rotation varimax automatiquement
print(factanal(dl0[, -1], factors = 2, rotation = "varimax"),cutoff = 0) #fait une rotation varimax automatiquement
print(factanal(dl0[, -1], factors = 1, rotation = "varimax"),cutoff = 0) #fait une rotation varimax automatiquement

#A faire si temps ACP pour variables et sujets (et se demander si normaliser ou non)

#dimension avec Q
dim1 <- c("HAMD4", "HAMD8", "HAMD12", "HAMD13", "HAMD14", "HAMD16")
dim2 <- c("HAMD1", "HAMD2", "HAMD3", "HAMD5", "HAMD6", "HAMD7", "HAMD9",
          "HAMD10", "HAMD11", "HAMD15", "HAMD17")


#C- cohérence interne
cronbach(dl0[,-1])
cronbach(dl0[,dim1])
cronbach(dl0[,dim2])

#d- validité concourrante et divergente
sc.dim1 <- paste0("Q",c(1,4,12,27,42,48,49,52,53,56,58,40))
sc.dim2 <- paste0("Q",c(9,10,28,38,3,4,46,51,55,65))
sc.dim3 <- paste0("Q",c(6,21,34,36,37,41,61,69,73))
sc.dim4 <- paste0("Q",c(5,14,15,20,22,26,29,30,31,32,54,71,79))
sc.dim5 <- paste0("Q",c(2,17,23,33,39,57,72,78,80,86))
sc.dim6 <- paste0("Q",c(11,24,63,67,74,81))
sc.dim7 <- paste0("Q",c(13,25,47,70,75,82,50))
sc.dim8 <- paste0("Q",c(8,18,43,68,76,83))

dl0$global <- apply(dl0[,-1],1,sum) #de 0 à 52
for (i in 1:8){
  #browser()
  sc0[ ,paste0("dim",i)] <- apply(sc0[,get(paste0("sc.dim",i))],1,function(x)sum(x,na.rm=T)/length(!is.na(x))) #que faire des NA?
}

#corrélation:
sapply(1:8,function(i)cor(dl0$global,sc0[ ,paste0("dim",i)])) #j'ai bien fait attention à ce que les numero de dl0 et sc0 soit dans le meme ordre
#corrèle le plus avec dimension 1(somatisation), 2(symptomes obsessionels), 4(dépression)



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

#A2- nb de NA
#qplot aurait affiché les NA s'il y en avait, mais 2 vérifications vallent mieux qu'une
#qplot(as.factor(c(1,1,2,2,2,3,3,1,NA)))#qplot représente automatiquement les NA
apply(dl56,2,function(x)sum(is.na(x))) #aucun NA

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
print(factanal(dl56[, -1], factors = 3, rotation = "varimax"),cutoff = 0) #fait une rotation varimax automatiquement
print(factanal(dl56[, -1], factors = 2, rotation = "varimax"),cutoff = 0) #fait une rotation varimax automatiquement
print(factanal(dl56[, -1], factors = 1, rotation = "varimax"),cutoff = 0) #fait une rotation varimax automatiquement

#A faire si temps ACP pour variables et sujets (et se demander si normaliser ou non)

#dimension avec Q
dim1 <- c("HAMD4", "HAMD8", "HAMD12", "HAMD13", "HAMD14", "HAMD16")
dim2 <- c("HAMD1", "HAMD2", "HAMD3", "HAMD5", "HAMD6", "HAMD7", "HAMD9",
          "HAMD10", "HAMD11", "HAMD15", "HAMD17")


#C- cohérence interne
cronbach(dl56[,-1])
cronbach(dl56[,dim1])
cronbach(dl56[,dim2])

#d- validité concourrante et divergente
sc.dim1 <- paste0("Q",c(1,4,12,27,42,48,49,52,53,56,58,40))
sc.dim2 <- paste0("Q",c(9,10,28,38,3,4,46,51,55,65))
sc.dim3 <- paste0("Q",c(6,21,34,36,37,41,61,69,73))
sc.dim4 <- paste0("Q",c(5,14,15,20,22,26,29,30,31,32,54,71,79))
sc.dim5 <- paste0("Q",c(2,17,23,33,39,57,72,78,80,86))
sc.dim6 <- paste0("Q",c(11,24,63,67,74,81))
sc.dim7 <- paste0("Q",c(13,25,47,70,75,82,50))
sc.dim8 <- paste0("Q",c(8,18,43,68,76,83))

dl56$global <- apply(dl56[,-1],1,sum) #de 0 à 52
for (i in 1:8){
  #browser()
  sc56[ ,paste0("dim",i)] <- apply(sc56[,get(paste0("sc.dim",i))],1,function(x)sum(x,na.rm=T)/length(!is.na(x))) #que faire des NA?
}

#corrélation:
sapply(1:8,function(i)cor(dl56$global,sc56[ ,paste0("dim",i)])) #j'ai bien fait attention à ce que les numero de dl56 et sc56 soit dans le meme ordre
#corrèle le plus avec dimension 1(somatisation), 2(symptomes obsessionels), 4(dépression)