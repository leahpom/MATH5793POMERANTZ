#' @title PCA of the Sample Covariance
#'
#' @param data a data frame
#'
#' @return sample.Cov, the sample covariance
#' @return PC.Sigma, the principle components (the coefficients)
#' @return rho_yixk, the matrix of the correlation between each principle component and each column of data
#' @return TSV, the total sample variance
#' @export
#'
#' @examples
#' data <- datasets::mtcars
#' pca_sigma(data[,1:3])
pca_sigma<- function(data){ # data is a data.frame
  # SIGMA
  S <- cov(data)

  # principal components for Sigma - go to output
  esig <- eigen(S)
  pcsig <- as.matrix(esig$vectors)
  nc <- ncol(data)
  rcol <- 1:nc
  pcsigRet <- t(pcsig) # return the transpose for clarity
  rownames(pcsigRet) <- c(matrix(c(paste("e'_", rcol)), nrow = nc, ncol = 1)) # giving it row names for clarity

  # eigenvalues
  evalues <- esig$values

  # rho(yi, xj) - go to output
  n <- nrow(S) # same as the number of columns

  ryixk <-  matrix(rep(0, n*n), nrow = n, ncol = n)
  for (i in 1:n) {
    for(k in 1:n){
      ryixk[i,k] <- (pcsig[i,k] * sqrt(evalues[i]))/sqrt(S[k,k])
    }
  }
  # row and column labels to make it look nice
  r1rho <- rep(c(1:n), n)
  rownames(ryixk) <- c(matrix(c(paste("y_", r1rho)), nrow = n, ncol = 1))
  colnames(ryixk) <- c(matrix(c(paste("x_", r1rho)), nrow = 1, ncol = n))

  # sum(lambdas) - to to output
  TSV <- sum(esig$values)


  # OUTPUT
  outList <- list(sample.Cov = S, PC.Sigma = pcsigRet, rho_yixk = ryixk, TSV = TSV)

  return(outList)
}
