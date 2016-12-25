source("src/objects_outils.r")

#analyse outils Q2 

#Imputation par LOCF
table(is.na(h0$global))

head(h0)
head(dl)

dlh <- dl[,1:20]

dlh$global <- apply(dlh[,4:ncol(dlh)],1,sum)
summary(dlh$global)

time <- as.numeric(names(table(dl$time)))

# LOCF <- function(data,vec_time){
#   .dat <- data %>% filter (is.na(data$global))
#   newdat <- mapply(.dat$NUMERO,.dat$time,function(x){
#     browser()
#   })
#   #browser()
# }
# LOCF (dlh)

dwh <- dw[,!colnames(dw) %in% colnames(dw)[grep("Q",colnames(dw))]]
dwh[ ,paste0("tot_",time)] <- sapply(paste0("_",time), function(i) {
  #browser()
  res <- apply(dwh[,grep(i,colnames(dwh))],1,sum)
  return(res)
  })
dwh <- dwh[,c("NUMERO","GROUPE",colnames(dwh)[grep("tot_",colnames(dwh))])]


getLOCF <- function(line){
  #browser()
  .l <- line
  .l <- .l[!is.na(.l)]
  .l <- if(length(.l)==0) NA else tail(.l,1)
  return(.l)
}
dwh$last <- apply(dwh[,grep("tot_",colnames(dwh))],1,getLOCF)

for (i in paste0("tot_",time)){
  dwh[ ,i] <- ifelse (is.na(dwh[ ,i]), dwh$last,dwh[ ,i])
}

# dwh[ ,paste0("tot_",time)] <- sapply(paste0("tot_",time), function(i) {
#   #browser()
#   res <- if(is.na(dwh[,grep(i,colnames(dwh))])) dwh$last else dwh[,grep(i,colnames(dwh))]
#   return(res)
# })

t.test (dwh[dwh$GROUPE=="0","last"],dwh[dwh$GROUPE=="1","last"])
