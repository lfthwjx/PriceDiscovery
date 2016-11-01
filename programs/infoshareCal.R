library(ifrogs)
library(FinTS)
library(xlsx)
library(quantmod)
library(timeSeries)

pdshare.updated<-function (x, override.lags = NULL, lag.max = 10) 
{
  stopifnot(ncol(x) == 2)
  stopifnot(is.numeric(x[, 1]))
  stopifnot(is.numeric(x[, 2]))
  if (is.null(override.lags)) {
    if (lag.max < 2) 
      stop("Minimum lags should be 2")
  }
  else {
    if (override.lags < 2) 
      stop("Minimum lags should be 2")
  }
  cnames <- colnames(x)
  pdshare.computation <- function(x, nlag) {
    cointest <- ca.jo(x, K = nlag, type = "eigen", ecdet = "const", 
                      spec = "transitory")
    k <- cointest@lag
    vecm <- cajorls(cointest)
    varm <- vec2var(cointest)
    vma <- Psi(varm)
    beta.ort <- as.vector(c(-cointest@V[2, 1], cointest@V[1, 
                                                          1]))
    alpha.ort <- as.vector(c(-cointest@W[2, 1], cointest@W[1, 
                                                           1]))
    aa <- bb <- cc <- dd <- 0
    for (i in 1:(k - 1)) {
      aa <- aa + vecm$rlm$coefficients[2 * i, 1]
      bb <- bb + vecm$rlm$coefficients[2 * i + 1, 1]
      cc <- cc + vecm$rlm$coefficients[2 * i, 2]
      dd <- dd + vecm$rlm$coefficients[2 * i + 1, 2]
    }
    gamma.1 <- matrix(c(1 - aa, -bb, -cc, 1 - dd), nrow = 2, 
                      ncol = 2, byrow = TRUE)
    b <- as.numeric(t(alpha.ort) %*% gamma.1 %*% beta.ort)
    psi <- (beta.ort %*% t(alpha.ort))/b
    f <- vma[, , 1]
    omega <- f %*% t(f)
    psi <- t(psi[1, ])
    n <- psi %*% f
    d <- psi %*% omega %*% t(psi)
    list(ishares = c((n[, 1]^2)/d, (n[, 2]^2)/d), alpha.ort = alpha.ort, 
         omega = omega, lags = varm$p)
  }
  if (is.null(override.lags)) {
    nlag <- VARselect(x, lag.max = lag.max)$selection[1]
    if (nlag<2){nlag<-2}
  }
  else {
    nlag <- override.lags
  }
  tmp <- pdshare.computation(x, nlag)
  is.original.ordering <- as.data.frame(tmp$ishares)
  component.share <- as.data.frame(abs(tmp$alpha.ort)/sum(abs(tmp$alpha.ort)))
  var.covar.matrix <- tmp$omega
  lags.used <- tmp$lags
  tmp <- pdshare.computation(x[, c(2, 1)], nlag)
  is.reversed.ordering <- as.data.frame(tmp$ishares)
  rownames(var.covar.matrix) <- colnames(var.covar.matrix) <- rownames(component.share) <- rownames(is.original.ordering) <- cnames
  rownames(is.reversed.ordering) <- c(cnames[2], cnames[1])
  colnames(is.original.ordering) <- colnames(is.reversed.ordering) <- "IS"
  colnames(component.share) <- "CS"
  list(is.original.ordering = is.original.ordering, is.reversed.ordering = is.reversed.ordering, 
       component.share = component.share, var.covar.matrix = var.covar.matrix, 
       lags.used = lags.used)
}

setwd("D:\\UM\\calGARCH\\macro uncertainty 2nd")
fdouyou_daily<-read.xlsx("D:\\UM\\calGARCH\\macro uncertainty 2nd\\fdouyou 0804.xls", sheetIndex=1,startRow = 3, endRow = 2029, header=FALSE)
sdouyou_daily<-read.xlsx("D:\\UM\\calGARCH\\macro uncertainty 2nd\\sdouyou 0804.xls", sheetIndex=1,startRow = 3, endRow = 2057, header=FALSE)

fdouyou <- xts(log(fdouyou_daily[,2]), order.by=fdouyou_daily[,1])
sdouyou <- xts(log(sdouyou_daily[,2]), order.by=sdouyou_daily[,1])
fpoints_quarters<-endpoints(fdouyou, on='quarters', k=1)
fpoints_quarters<-fpoints_quarters[1:(length(fpoints_quarters)-1)]
spoints_quarters<-endpoints(sdouyou, on='quarters', k=1)
spoints_quarters<-spoints_quarters[1:(length(spoints_quarters)-1)]
fnum_quarters<-length(fpoints_quarters)
snum_quarters<-length(spoints_quarters)
if(identical(fnum_quarters,snum_quarters)){
infoshare<-matrix(c(1:(8*(fnum_quarters-1))),nrow = fnum_quarters-1,ncol = 8)
num_quarters<-fnum_quarters
num_quarters
for (i in 1:(num_quarters-1)) {
  #i=33
  print(i)
  fu=fpoints_quarters[i]+1
  fu
  fb=fpoints_quarters[i+1]
  fb
  fdouyou_quarterly<-fdouyou[c(fu:fb)]
  #fdouyou_quarterly
  su=spoints_quarters[i]+1
  sb=spoints_quarters[i+1]
  sdouyou_quarterly<-sdouyou[c(su:sb)]
  #sdouyou_quarterly
  #common_douyou<-na.omit(merge(sdouyou_quarterly,fdouyou_quarterly))
  common_douyou<-merge(sdouyou_quarterly,fdouyou_quarterly)
  #common_douyou
  common_douyou<-common_douyou[complete.cases(common_douyou),]
  # str(common_douyou)
  #common_douyou_ts<-as.ts(common_douyou)
  #common_douyou_ts
  #ma<-matrix(common_douyou,ncol = 2)
  #result_is<-pdshare(common_douyou,override.lags=2)
  result_is<-pdshare.updated(common_douyou)
  #lag.max<-10
  #nlag <- MVARselect(x, lag.max = lag.max)$selection[1]
  #str(ma)
  quarterly_is<-result_is$is.original.ordering
  quarterly_is_reversed<-result_is$is.reversed.ordering
  quarterly_cs<-result_is$component.share
  infoshare[i,c(1:2)]<-t(quarterly_is)
  infoshare[i,c(3:4)]<-t(quarterly_is_reversed)
  infoshare[i,5]<-((infoshare[i,1]+infoshare[i,4])/2)
  infoshare[i,6]<-((infoshare[i,2]+infoshare[i,3])/2)
  infoshare[i,c(7:8)]<-t(quarterly_cs)
}

}
#View(infoshare)
cnames<-c("is.original.ordering.spot","is.original.ordering.futures",
          "is.reversed.ordering.futures", "is.reversed.ordering.spot",
          "mean of the spot","mean of the futures",
          "component.share.spot","component.share.futures")
colnames(infoshare)<-cnames
infoshare.ts<-ts(infoshare,frequency = 4,start = c(2008,2))
infoshare.ts<-as.timeSeries(infoshare.ts)
write.xlsx(infoshare.ts, "D:\\UM\\calGARCH\\output\\infoshare_douyou.xlsx",sheetName = "infoshare douyou spot and futures", 
           col.names = TRUE, row.names = TRUE)
