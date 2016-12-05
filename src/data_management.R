gpe <- read.csv2("data/groupe.csv")
hdrs <- read.csv2("data/hdrs.csv")
scl <- read.csv2("data/SCL90.csv")

gpe$NUMERO <- as.integer(gpe$NUMERO)
gpe$GROUPE <- as.factor(gpe$GROUPE)

summary(hdrs)
hdrs$NUMERO <- as.integer(hdrs$NUMERO)
for (i in colnames(hdrs)[grep("HAM",colnames(hdrs))]){
  hdrs[,i] <- as.factor(hdrs[ ,i]) 
}

scl$NUMERO <- as.integer(scl$NUMERO)
summary(scl)
for (i in colnames(scl)[grep("Q",colnames(scl))]){
  scl[,i] <- as.factor(scl[ ,i]) 
}


d <- merge(hdrs, scl, by=c("NUMERO","VISIT"),all.x=T,all.y=T)
d <- merge(d,gpe,by="NUMERO",all.x=T)


#Combien de patients avec 1 mesure,2,3,4,5,6,7 ou 8 mesures
table(table(d$NUMERO))

reshape