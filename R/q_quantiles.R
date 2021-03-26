#' @title Data Quantiles
#'
#' @param x a column of data
#'
#' @return orderedObs a vector containing the entered data, in ascending order
#' @return probabilityLevels a vector containing the probability level for each ordered observation
#' @return stndNormQuant the standard normal quantile, used for creating a QQ-Plot
#'
#' @export
#'
#' @importFrom stats qnorm
#'
#' @examples
#' x <- c(-1.00, -.10, .16, .41, .62, .80, 1.26, 1.54, 1.71, 2.30)
#' q_quantiles(x)
#'
q_quantiles <- function(x){ # x is a column of data
  # make sure x is in the right format
  x <- as.matrix(x)
  n <- nrow(x) # calculate the number of times to run the loop

  # order the observations
  xOrder <- sort(x)

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
    qValues[k] <- qnorm(probLevels[k])
  }

  # bind each column into a list, return the list
  varList <- list(orderedObs = xOrder, probabilityLevels = probLevels, stndNormQuant = qValues)
  return(varList)

}
