get_items_correlation <- function (data){
  mat <- cor(data[,-1], use="complete.obs")
  
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
  return(couplesup0.2)  
}

getLOCF <- function(line){
  #browser()
  .l <- line
  .l <- .l[!is.na(.l)]
  .l <- if(length(.l)==0) NA else tail(.l,1)
  return(.l)
}



#BOOTSTRAP DE CRONBACH (attention utilise library(psy))
#ma fonction statistique
cronbach.boot<- function(data,indices){
  .dat<- data[indices,]
  cron <- cronbach(.dat[,-1])$alpha
  return(cron)
}
#ma fonction de bootstrap
bootcron<- function (df,n_repet)  {    
  .res<-boot(data=df,statistic = cronbach.boot ,R=n_repet)
  return(.res)
}
a <- bootcron(dl0,10)

#ma fonction d'intervalle de confiance
BootCronCi <- function(.data,.R)  {
  .bootres <- bootcron (df=.data, n_repet=.R)
  #browser()
  .list.ci <- boot.ci(.bootres,index=1,type="perc")
  .res <- data.frame (t(.list.ci[["percent"]][4:5]))
  colnames (.res) <- c ("CI_L", "CI_U")
  .res$est <- as.numeric (.bootres$t0)
  .res$n <- sum(!is.na(apply(.data[,-1],1,sum,na.rm=F))) #Je fais la somme des items et je regarde si des sommes sont NA 
  .res <- .res[, c (4,3, 1, 2)] #Je réordonne mes colonnes
  .ans <- round (.res, 2) #fait un arrondi sur chaque valeur
  .ans <- data.frame (N=.res$n, alpha_CI=paste0 (.ans$est, " [", .ans$CI_L, "-", .ans$CI_U, "]")) #met en forme les valeurs
  return (.ans)
}
