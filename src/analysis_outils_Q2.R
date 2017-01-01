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

for (i in paste0("tot_", c(0, 4, 7, 14, 21, 28, 42, 56))){
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

#pour les données
head(dlh)
summary(mod1 <- lm(tot~time*GROUPE+NUMERO,dlh)) #interaction significative
summary(mod2 <- lmer(tot~time*GROUPE+(1|NUMERO),dlh))

#normalité des résidus
qplot(resid(mod2))

#pour interpreter la t value, je cherche le nombre de degrés de libertés

# get the KR-approximated degrees of freedom
df.KR <- get_ddf_Lb(mod2, fixef(mod2)) #require(pbkrtest)
coefs <- data.frame(coef(summary(mod2)))
# get p-values from the t-distribution using the t-values and approximated degrees of freedom
coefs$p.KR <- 2 * (1 - pt(abs(coefs$t.value), df.KR))
coefs$p.KR <- 2 * (1 - pt(abs(coefs$t.value), 145))
# use normal distribution to approximate p-value
coefs$p.z <- 2 * (1 - pnorm(abs(coefs$t.value)))
coefs