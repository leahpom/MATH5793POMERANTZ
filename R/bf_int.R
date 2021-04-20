#' @title Bonferroni Confidence Intervals for \eqn{\lambda} Values
#'
#' @param data  a data frame
#' @param q the number of variances we want to find confidence intervals for. We find confidence intervals for the first 1:q variances
#' @param alpha the significance level, default set to the standard 0.05
#'
#' @description We use the formula given on pages 456 and 457 of our book \emph{Applied Multivariate Statistical Analysis},
#'  6th Edition, by Johnson and Wichern, to create our formula for the confidence intervals.
#'
#' @return ci, an object containing the q confidence intervals as a matrix
#' @export
#'
#' @examples
#' data <- datasets::mtcars
#' bf_int(data, q = 2)
#'
bf_int <- function(data, q, alpha = 0.05){

  S <- cov(data) # construct the covariance matrix
  eigenS <- eigen(S) # find the eigenvectors and eigenvalues
  lambdas <- eigenS$values # separate off the eigenvalues
  lambdas_int <- lambdas[1:q] # separate off the ones we are actually going to find the intervals for
  p <- ncol(data) # this is the "m" in the formula
  n <- nrow(data) # this is the number of rows, which we use in the formula

  # create a matrix to store the values for nice output
  ci <- rep(0, 2*q)
  ci <- matrix(ci, nrow = q, ncol = 2) # empty matrix to store values

  # column labels to make it look nice
  lb_lab <- as.character(100*(alpha/2))
  ub_lab <- as.character(100*(1-alpha/2))
  colnames(ci) <- c(paste(lb_lab, "%"), paste(ub_lab, "%"))

  # calcualte the intervals
  for (i in 1:q) {
    z <- qnorm(1-alpha/(2*p)) # the z-value, calculated here to improve readability in the code
    ci[i,1] <- lambdas_int[i]/(1 + z*sqrt(2/n)) # the lower bound
    ci[i,2] <- lambdas_int[i]/(1 - z*sqrt(2/n)) # the upper bound

  }

  # return statement
  return(ci)

}
