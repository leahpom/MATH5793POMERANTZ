#' @title Values for Chi-Square Plot
#'
#' @description This function is for use in my shiny_MVN app to calculate the
#' values for the chi-square plot for assessing normality
#'
#' @param df a bivariate data.frame
#'
#' @return j the observation number, ordered by the squared distance
#' @return d_jsq the squared distance
#' @return q_j the chi-square quantiles from the chi-square distribution
#'
#' @importFrom stats qchisq
#'
#' @export
#'
#' @examples
#' x1 <- c(108.28, 152.36, 95.04, 65.45, 62.97, 263.99, 265.19, 285.06, 92.01, 165.68)
#' x2 <- c(17.05, 16.59, 10.91, 14.14, 9.52, 25.33, 18.54, 15.73, 8.10, 11.13)
#' data <- data.frame(cbind(x1, x2))
#' chi_sqQ(data)
chi_sqQ <- function(df){ # df is a bivariate data set
  # make sure x is in the right format
  df <- as.data.frame(df)
  n <- nrow(df) # calculate the number of times to run the loop

  # calculate the distances
  xBar <- colMeans(df)
  xBar <- matrix(c(rep(xBar[1], n), rep(xBar[2], n)), nrow = n, ncol = 2)
  xbmb <- df - xBar
  S <- cov(df)
  Sinv <- solve(S)

  dSq <- rep(0, n) # create matrix to hold the values
  for (i in 1:n) {
    obs_i <- as.matrix(xbmb[i, ])
    dSq[i] <- obs_i %*% Sinv %*% t(obs_i)
  }

  # order the distances observations
  ordDist <- sort(dSq)

  # create a column with the number of the ordered observation
  jObs <- rep(0, n) # create an empty vector to store the order of the observations, j
  for (i in 1:n){
    jObs[i] <- i
  }

  # calculate the probability levels, (j - 0.5)/n
  probLevels <- rep(0, n) # create an empty vector to store the probability levels
  for(j in 1:n){
    probLevels[j] <- (jObs[j] - 0.5)/n
  }

  # calculate the q-values, qnorm(probability levels)
  qValues <- rep(0, n) # create an empty vector to store the probability levels
  for (k in 1:n) {
    qValues[k] <- qchisq(probLevels[k], 2)
  }

  # bind each column into a data frame and a list, return the list
  varList <- list(j = jObs, d_jsq = ordDist, q_j = qValues)
  return(varList)

}
