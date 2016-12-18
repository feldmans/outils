f1 <- read.csv2("c:/fichier 1.csv")
f2 <- read.csv2("c:/fichier 2.csv")
f3 <- read.csv2("c:/fichier 3.csv")
f4 <- read.csv2("c:/fichier 4.csv")

# pour des patients identiques (index�s par NUDO), fusionne deux jeux avec des variables diff�rentes
f13 <- merge(f1,f3,by="NUDO")

# pour des patients identiques (index�s par NUDO), fusionne deux jeux avec des variables diff�rentes
f24 <- merge(f2,f4,by="NUDO")

# pour des variables identiques, fusionne deux jeux avec des sujets diff�rents
ft <- rbind(f13,f24)

# transforme un fichier "sujet" en un fichier "consultation"
fint <- reshape(ft,direction="long",varying=list(c(2,4,6,8),c(3,5,7,9)),times=c(0,1,3,6),idvar="NUDO",v.names=c("I","J"))
flong <- fint[,c(1,10,11,12,13,14)]

# transforme un fichier "consultation" en un fichier "sujet"
fwide <- reshape(flong,direction="wide",times=c(0,1,3,6),idvar="NUDO",v.names=c("I","J"))

# change des noms de variables
dimnames(ft)[[2]][2:9] <- c("I.0","J.0","I.1","J.1","I.2","J.2","I.3","J.3")

# D�coupe une variable continue en une variable discr�te
ft$agecat <- cut(ft$age,breaks=c(-Inf,25,55,75,Inf),labels=1:4)
# Transforme une variable cat�gorielle en une variable quantitative
ft$agecat <- as.numeric(levels(ft$agecat))[ft$agecat]
# Transforme une variable quantitative en une variable cat�gorielle
ft$agecat <- as.factor(ft$agecat)


