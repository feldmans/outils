# Attention : ce n'est pas comme pour le LOCF, si je n'ai pas de valeur c'est une censure (mais je n'impute pas)

source ("src/objects_outils.R")


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




