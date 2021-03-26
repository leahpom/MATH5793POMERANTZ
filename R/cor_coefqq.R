#' @title Correlation Coefficient
#'
#' @param x the column of a data frame
#'
#' @return rq the correlation coefficient for the given column
#'
#' @export
#'
#' @examples
#' \dontrun{
#' cor_coefqq(mtcars[,1])
#' }
cor_coefqq <- function(x){ # x is a column of a data frame

  # get x-bar
  # make sure x is in the right format
  x <- as.matrix(x)
  n <- nrow(x)
  xbar <- mean(x)

  # get q using q_quantiles()
  quants <- MATH5793POMERANTZ::q_quantiles(x)
  qi <- quants$stndNormQuant

  # calculate q-bar
  qbar <- mean(qi)

  # calculate numerator
  xixb <- x - xbar
  qiqb <- qi - qbar

  numi <- rep(0, n) # empty vector to store the values

  for (i in 1:n) {
    numi[i] <- xixb[i] * qiqb[i]
  }

  numerator <- sum(numi) # final value for the numerator

  # calculate denominator
  denom1 <- sqrt(t(xixb) %*% xixb)
  denom2 <- sqrt(t(qiqb) %*% qiqb)

  # calcualte r_q

  rq <- numerator/(denom1 * denom2)

  return(rq)

}
