#' @title (1 - \eqn{\alpha})*100\% Bonferroni Confidence Intervals for \eqn{\mu}
#'
#' @param data a data.frame
#' @param alpha our \emph{overall} significance level, set to the standard 0.05
#'
#' @description The formula used to create this confidence interval cis 5-29 from page 232 of our book, \emph{Applied Multivariate Statistical Analysis},
#'  6th Edition, by Johnson and Wichern
#'
#' @return The \eqn{(1 - \alpha)*100}\% Bonferroni confidence intervals
#' @export
#'
#' @examples
#' testData <- datasets::mtcars
#' bfMean_int(data = testData)
#'
bfMean_int <- function(data, alpha = 0.05){
  # data is a data.frame
  # alpha is our OVERALL significance level, set to the standard 0.05

  # perform necessary calculations
  xbar <- colMeans(data)
  S <- cov(data)
  p <- ncol(data)
  n <- nrow(data)
  prob <- alpha/(2*p) # the probability level, since we're using a family error rate
  critT <- stats::qt(prob, df = n - 1, lower.tail = FALSE)

  # create a matrix to store the output - first column is lower bound, second column is upper bound
  ciMat <- matrix(c(rep(0, 2*p)), nrow = p, ncol = 2)
  # give nice column names
  lb_lab <- as.character(100*(alpha/2))
  ub_lab <- as.character(100*(1-alpha/2))
  colnames(ciMat) <- c(paste(lb_lab, "%"), paste(ub_lab, "%"))

  # calculate the intervals in a for-loop
  for (i in 1:p) {
    pmVal <- critT * sqrt(S[i,i]/n) # this is the value that gets added/substracted (plus/minus -> pm)
    ciMat[i,1] <- xbar[i] - pmVal
    ciMat[i,2] <- xbar[i] + pmVal
  }

  # return the matrix
  return(ciMat)

}
