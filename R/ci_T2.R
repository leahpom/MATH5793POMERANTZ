#' @title Simultaneous \eqn{T^2} Confidence Intervals
#'
#' @param data a data.frame
#' @param alpha the significance level for the intervals - default to standard 0.05
#'
#' @description We use the formula given in equation (5-24) on page 224 of our
#' book \emph{Applied Multivariate Statistical Analysis}, 6th Edition, by Johnson and Wichern, to create our
#' formula for the confidence intervals.
#'
#' @return the 100(\eqn{\alpha})\% confidence intervals based on \eqn{T^2}
#' @export
#'
#' @examples
#' testData <- datasets::mtcars[,1:4]
#' ci_T2(testData, 0.05)
ci_T2 <- function(data, alpha = 0.05){

  # necessary calculations for formula
  p <- ncol(data)
  n <- nrow(data)
  S <- cov(data)
  xbar <- colMeans(data)
  fVal <- qf(alpha, df1 = p, df2 = n-p, lower.tail = FALSE) # the necessary F value for each calculation
  num <- p*(n-1)
  denom <- n-p
  sqrtVal <- sqrt((num/denom)*fVal) # this number is used in each interval, so it's calculated outside of the loop

  # create a matrix to hold the calculations - first column is lower bound, second column is upper bound
  ciMat <- matrix(c(rep(0, 2*p)), nrow = p, ncol = 2)

  # calculate the LB and UB in a for-loop
  for (i in 1:p) {
    pmVal <- sqrtVal * sqrt(S[i,i]/n) # this is the value that gets added/substracted (plus/minus -> pm)
    ciMat[i,1] <- xbar[i] - pmVal
    ciMat[i,2] <- xbar[i] + pmVal
  }

  # give nice column names
  lb_lab <- as.character(100*(alpha/2))
  ub_lab <- as.character(100*(1-alpha/2))
  colnames(ciMat) <- c(paste(lb_lab, "%"), paste(ub_lab, "%"))

  # return the matrix
  return(ciMat) # we should have the name number of rows and the variables that we looked at

}
