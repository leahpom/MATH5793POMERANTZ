#' @title Standardized Score (Z-scores) for a data frame
#'
#' @param data a data.frame of continuous variables
#'
#' @return Z a matrix of the standardized variables for each entry in the data frame
#' @export
#'
#' @examples
#' stnd_values(mtcars)
stnd_values <- function(data){ # data is a data.frame

  xBar <- colMeans(data) # calculate the x-bar_k values for k in 1 to p
  S <- cov(data) # calculate the covariance matrix
  p <- ncol(data)
  n <- nrow(data)

  Z <- matrix(rep(0, n*p), nrow = n, ncol = p) # create an empty matrix to hold the standardized values

  for (k in 1:p) {
    # create necessary values
    xBar_k <- xBar[k]
    s_kk <- S[k,k]

    # calculate each z value and store in the matrix
    for (j in 1:n) {
      x_jk <- data[j,k]
      Z[j,k] <- (x_jk - xBar_k) / sqrt(s_kk)
    }
  }

  return(Z) # return the standardized normal values

}
