#' @title \eqn{T^2} Confidence Interval for the Difference in Means
#'
#' @param data a data.frame
#' @param alpha the significance level, set to the standard 0.05
#' @param i the first column in our data frame that we want to use in our calculation
#' @param k the second column in our data frame that we want to use in our calculation
#'
#' @description We use equation 5-25, found on page 226 of our book, \emph{Applied Multivariate Statistical Analysis},
#'  6th Edition, by Johnson and Wichern
#'
#' @return The \eqn{(1 - \alpha)*100}\% confidence interval for the difference in means, \eqn{\mu_i - \mu_k}
#' @export
#'
#' @examples
#' testData <- datasets::mtcars
#' ciDiffMeans(data = testData, i = 1, k = 2)
#'
ciDiffMeans <- function(data, alpha = 0.05, i = 1, k = 2){

  # necessary calculations for formula
  p <- ncol(data)
  n <- nrow(data)
  S <- cov(data)
  xbar <- colMeans(data)
  fVal <- stats::qf(alpha, df1 = p, df2 = n-p, lower.tail = FALSE) # the necessary F value for each calculation
  num <- p*(n-1)
  denom <- n-p
  sqrtVal1 <- sqrt((num/denom)*fVal)
  sqrtVal2 <- sqrt((S[i,i] - 2*S[i,k] + S[k,k])/n)

  # create a matrix to hold the calculations - first column is lower bound, second column is upper bound
  ciMat <- matrix(c(rep(0, 2)), nrow = 1, ncol = 2)

  # calculate the interval
  pmVal <- sqrtVal1 * sqrtVal2 # this is the value that gets added/subtracted (plus/minus -> pm)
  ciMat[1,1] <- (xbar[i] - xbar[k]) - pmVal
  ciMat[1,2] <- (xbar[i] - xbar[k]) + pmVal

  # give nice column names
  lb_lab <- as.character(100*(alpha/2))
  ub_lab <- as.character(100*(1-alpha/2))
  colnames(ciMat) <- c(paste(lb_lab, "%"), paste(ub_lab, "%"))

  # return the matrix
  return(ciMat)

}
