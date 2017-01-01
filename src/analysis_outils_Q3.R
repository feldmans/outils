# Attention : ce n'est pas comme pour le LOCF, si je n'ai pas de valeur c'est une censure (mais je n'impute pas)

source ("src/objects_outils.R")

head(dwh)


#creation d'un tableau pour les données de survie (dws comme data, wide, survie)
dws <- dwh

dwh %>% filter(tot_4 < tot_0/2) 

#evenement oui non et date de survenue
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
dws$evt <- ifelse(!is.na(dws$date_evt),1,0)

#date des dernières nouvelles : 
#dernier questionnaire disponible si pas d'evenement
#date de l'evement si il y a eu evenement

#pour des raisons pratiques, je cherche la date des dernières nouvelles pour tout le monde
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



#Le patient ne peut pas avoir d'evenement entre J0 et J4 (est-ce qu'on appelle biais d'immortalité?),
#je ne sais pas comment le prendre en compte dans mon analyse, et j'ai peur de faire des erreurs, je garde donc
#mes durees de suivi telle quelle.



#Analyse de survie

#pour une analyse de survie, je n'ai besoin que de l'evt, de la duree de suivi,
#et bien sûr de l'identifiant du patient et de son groupe
dws <- dws[,c("NUMERO","GROUPE","evt","time")]

saveRDS (dws, "data/dws.rds")

#Nombre d'évènements par groupe
table(dws$evt,dws$GROUPE,deparse.level = 2)
#nombre d'évènements à chaque visite
table(dws$time[dws$evt==1])

#Analyses preliminaires : Description globale (sans differencier les courbes)

#suivi 
suiv <- survfit(Surv(dws$time,1-dws$evt)~1)
#plot du suivi global
plot(suiv,xscale=1, yscale= 100, xlab="Durée (jours)",xaxt = "n")
axis(1, at = levels(as.factor(dws$time)), cex.axis = 1) 
med <- min(suiv$time[suiv$surv<=0.5])#mediane de suivi

dws %>% filter(time==56 & evt==1)#10 evenement à J56

#survie
surv <- survfit(Surv(dws$time,dws$evt)~1, conf.int=.95)
#plot de la survie globale
plot(surv,xscale=1, yscale= 100, xlab="Durée (jours)",xaxt = "n")
axis(1, at = levels(as.factor(dws$time)), cex.axis = 1)
med1 <- min(surv$time[surv$surv<=0.5])#mediane de survie



#Comparaison de la survie entre les 2 groupes

#Courbes de survie
surv.grp <- survfit(Surv(dws$time,dws$evt)~dws$GROUPE, conf.int=.95)
plot(surv.grp, yscale= 100, col=c(2,4), xlab = "duree (jours)",xaxt = "n")
axis(1, at = levels(as.factor(dws$time)), cex.axis = 1)
legend (40,1,c(levels(as.factor(dws$GROUPE))),lty=c(1,1),col=c(2,4),title="Groupe :")

#On peut utiliser un test du log rank qui est non paramétrique et ne fait donc pas d'hypothèse sur la 
#distribution de la survie. Comme la comparaison se fait entre 2 groupes, on pourra calculer également le HR.

#Test du logrank
sdf <- survdiff(Surv(dws$time,dws$evt)~dws$GROUPE)
p.val <- round(1 - pchisq(sdf$chisq, length(sdf$n) - 1),5) #2e-05
#HR à partir des résultats du logrank
HR <- (sdf$obs[2]/sdf$exp[2])/(sdf$obs[1]/sdf$exp[1])
up95 <- exp(log(HR) + qnorm(0.975)*sqrt(1/sdf$exp[2]+1/sdf$exp[1]))
low95 <- exp(log(HR) - qnorm(0.975)*sqrt(1/sdf$exp[2]+1/sdf$exp[1]))


#On peut également comparer les groupes par modèle de Cox

#conditions :
#1:risques proportionnels
a <- cox.zph(coxph(Surv(dws$time,dws$evt)~dws$GROUPE)) 
#pvalue:
a$table[3]
#non significatif mais peut être manque de puissance => je trace une courbe
plot(a,main="GROUPE")
#la courbe est à peu près horizontale, j'en conclue donc que la condition est vérifiée

#J'ai essayé de transformer en ggplot2 mais je n'y parviens pas.


#2:Loglinéarité : ?

#comparaison
a <- coxph(Surv(dws$time,dws$evt)~dws$GROUPE)

HR <- round(exp(coef(a)),2)
up95 <- round(exp(confint(a)),2)[1]
low95 <-round(exp(confint(a)),2)[2]
a<-summary(a)
res <- round(as.numeric(a$sctest[3]),3)




