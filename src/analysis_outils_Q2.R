source("src/objects_outils.r")

#analyse outils Q2 

#Imputation par LOCF
table(is.na(h0$tot)) #tous les score sont calculés à h0

#LOCF
getLOCF <- function(line){
  .l <- line
  .l <- .l[!is.na(.l)]
  .l <- if(length(.l)==0) NA else tail(.l,1)
  return(.l)
}
dwh$last <- apply(dwh[,grep("tot_",colnames(dwh))],1,getLOCF)

for (i in paste0("tot_",vartime)){
  dwh[ ,i] <- ifelse (is.na(dwh[ ,i]), dwh$last,dwh[ ,i])
}

dwh$diff_56_0 <- dwh$last - dwh$tot_0 

#Test de student
t.test (dwh[dwh$GROUPE=="0","diff_56_0"],dwh[dwh$GROUPE=="1","diff_56_0"])
#normalité de la différence de score
hist(dwh$diff_56_0)
#égalité des variances
by(dwh$diff_56_0,dwh$GROUPE,sd,na.rm=T)

#Régression linéaire
summary(moddiff <- lm(diff_56_0~GROUPE, dwh))
#normalité des résidus
hist(residuals(moddiff))

#modele mixte :

#exemple de la vidéo :
library(lme4)
set.seed(5)
x <- floor (20*rnorm(15))+60
y <- floor (20*rnorm(15))
x1 <- x
x2 <- x+y+7
dt <- data.frame(1:15,x1,x2)
names(dt)[1]<- "sujet"
dtl <- reshape(dt, idvar="sujet", varying=2:3,v.names="score",timevar = "temps", direction="long")

#dt <- data.frame(1:15,x1,x1,x2,x2)
#dtl <- reshape(dt, idvar="sujet", varying=list(c(2,4),c(3,5)), v.names=c("score1","score2"),timevar = "temps", direction="long")
#varying = pour chaque variable c(tps1,tps2)
#v.names = nom de chaque variable qui sera répétée à plsieurs temps

dtl$sujet <- as.factor(dtl$sujet)
summary(lm(score~temps+sujet,dtl))
summary(lmer(score~temps+(1|sujet),dtl))#voir comment on trouve la pvalue pour t test avec N-1 ddl

#pour les données
head(dlh)
summary(mod1 <- lm(tot~time*GROUPE+NUMERO,dlh)) #interaction significative
summary(mod2 <- lmer(tot~time*GROUPE+(1|NUMERO),dlh))


mod2 <- lmer(tot~time*GROUPE+(1|NUMERO),dlh)
qplot(resid(mod2))

#pour interpreter la t value, je cherche le nombre de degrés de libertés
require(pbkrtest)
# get the KR-approximated degrees of freedom
df.KR <- get_ddf_Lb(mod2, fixef(mod2))
coefs <- data.frame(coef(summary(mod2)))
# get p-values from the t-distribution using the t-values and approximated degrees of freedom
coefs$p.KR <- 2 * (1 - pt(abs(coefs$t.value), df.KR))
coefs$p.KR <- 2 * (1 - pt(abs(coefs$t.value), 145))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs