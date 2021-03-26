#' @title Drop-1 Quantiles
#'
#' @description This function will input
#'
#' @param x a column of data
#' @param dfDropPoints the observation to be dropped
#'
#' @return orderedObs a vector containing the entered data, in ascending order, with the specified observation dropped
#' @return probabilityLevels a vector containing the probability level for each ordered observation, calculated with the specified observation dropped
#' @return stndNormQuant the standard normal quantile, used for creating a QQ-Plot, calculated with the specified observation dropped
#' @export
#'
#' @examples
#' x <- 1:10
#' qq <- q_quantiles(x)
#' qd <- qq$stndNormQuant[1] # the observation to be dropped
#' xd <- qq$orderedObs[1]
#' dfDrop <- data.frame(xj = 1325, qj = qd)
#' drop1_quants(x = x, dfDropPoints = dfDrop)
drop1_quants <- function(x, dfDropPoints){ # x is a column of data, dfDropPoints is the observation to be dropped
  x <- as.data.frame(x)
  xDrop <- dplyr::filter(x, !x == dfDropPoints[1,1])
  qq <- q_quantiles(xDrop)
  return(qq)
}
