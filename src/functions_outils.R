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

#RISQUE PROPORTIONNELS EN GGPLOT (adapté de la fonction ggcoxzph)
ggcoxzph.1var <- function (fit, resid = TRUE, se = TRUE, df = 4, nsmo = 40, var2, 
                           point.col = "red", point.size = 1, point.shape = 19, point.alpha = 1, 
                           font.main = c(16, "plain", "black"), font.x = c(14, "plain", 
                                                                           "black"), font.y = c(14, "plain", "black"), font.tickslab = c(12, 
                                                                                                                                         "plain", "black"), ggtheme = theme_classic2()) 
{
  x <- fit
  if (!methods::is(x, "cox.zph")) 
    stop("Can't handle an object of class ", class(x))
  xx <- x$x
  yy <- x$y
  d <- nrow(yy)
  df <- max(df)
  nvar <- ncol(yy)
  pred.x <- seq(from = min(xx), to = max(xx), length = nsmo)
  temp <- c(pred.x, xx)
  lmat <- splines::ns(temp, df = df, intercept = TRUE)
  pmat <- lmat[1:nsmo, ]
  xmat <- lmat[-(1:nsmo), ]
  qmat <- qr(xmat)
  if (qmat$rank < df) 
    stop("Spline fit is singular, try a smaller degrees of freedom")
  if (se) {
    bk <- backsolve(qmat$qr[1:df, 1:df], diag(df))
    xtx <- bk %*% t(bk)
    seval <- d * ((pmat %*% xtx) * pmat) %*% rep(1, df)
  }
  #ylab <- paste("Beta(t) for", dimnames(yy)[[2]])
  ylab <- paste("Beta(t) for", var2)
  #if (missing(var)) 
  var <- 1:nvar
  # else {
  #   if (is.character(var)) 
  #    # browser()
  #     var <- match(var, dimnames(yy)[[2]])
  #   if (any(is.na(var)) || max(var) > nvar || min(var) < 
  #       1) 
  #     stop("Invalid variable requested")
  # }
  if (x$transform == "log") {
    xx <- exp(xx)
    pred.x <- exp(pred.x)
  }
  else if (x$transform != "identity") {
    #browser()
    xtime <- as.numeric(dimnames(yy)[[1]])
    indx <- !duplicated(xx)
    apr1 <- approx(xx[indx], xtime[indx], seq(min(xx), max(xx), 
                                              length = 17)[2 * (1:8)])
    temp <- signif(apr1$y, 2)
    apr2 <- approx(xtime[indx], xx[indx], temp)
    xaxisval <- apr2$y
    xaxislab <- rep("", 8)
    for (i in 1:8) xaxislab[i] <- format(temp[i])
  }
  #browser()
  plots <- list()
  plots <- lapply(var, function(i) {
    invisible(pval <- round(x$table[i, 3], 3))
    gplot <- ggplot() + ggtitle(paste0("Schoenfeld Individual Test p: ", 
                                       pval)) + ggtheme
    #browser()
    y <- yy[, i]
    yhat <- pmat %*% qr.coef(qmat, y)
    if (resid) 
      yr <- range(yhat, y)
    else yr <- range(yhat)
    if (se) {
      temp <- 2 * sqrt(x$var[i, i] * seval)
      yup <- yhat + temp
      ylow <- yhat - temp
      yr <- range(yr, yup, ylow)
    }
    if (x$transform == "identity") {
      gplot <- gplot + geom_line(aes(x = pred.x, y = yhat)) + 
        xlab("Time") + ylab(ylab[i]) + ylim(yr)
    }
    else if (x$transform == "log") {
      gplot <- gplot + geom_line(aes(x = log(pred.x), y = yhat)) + 
        xlab("Time") + ylab(ylab[i]) + ylim(yr)
    }
    else {
      gplot <- gplot + geom_line(aes(x = pred.x, y = yhat)) + 
        xlab("Time") + ylab(ylab[i]) + scale_x_continuous(breaks = xaxisval, 
                                                          labels = xaxislab) + ylim(yr)
    }
    if (resid) 
      gplot <- gplot + geom_point(aes(x = xx, y = y), col = point.col, 
                                  shape = point.shape, size = point.size, alpha = point.alpha)
    if (se) {
      gplot <- gplot + geom_line(aes(x = pred.x, y = yup), 
                                 lty = "dashed") + geom_line(aes(x = pred.x, y = ylow), 
                                                             lty = "dashed")
    }
    # gplot <- .labs(p = gplot, font.main = font.main, font.x = font.x, 
    #                font.y = font.y)
    # gplot <- .set_ticks(gplot, font.tickslab = font.tickslab)
  })
  #browser()
  names(plots) <- var
  #class(plots) <- c("ggcoxzph", "list")
  if(nrow(x$table)>1) attr(plots, "global_pval") <- x$table["GLOBAL", 3]
  plots
}
