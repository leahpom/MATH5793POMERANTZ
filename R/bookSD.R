#' Book Standard Deviation
#'
#' @param x a vector of quantitative data
#' @param n the number of observations in the vector
#'
#' @return standard deviation from the biased formula
#' @export
#'
#' @examples
#' x = 1:10
#' bookSDX <- bookSD(x, n=10)
bookSD <- function(x, n){
  sdNorm <- sqrt(stats::var(x)) # the normal standard deviation
  sdBook <- ((n-1)/n)*sdNorm # correcting for the biased formula
  sdBook # releases the standard deviation formula from the book
}
