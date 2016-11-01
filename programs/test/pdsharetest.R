function (x, override.lags = NULL, lag.max = 10) 
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
    nlag <- MVARselect(x, lag.max = lag.max)$selection[1]
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
<bytecode: 0x000000001f372620>
  <environment: namespace:ifrogs>