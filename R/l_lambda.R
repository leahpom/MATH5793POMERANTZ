#' @title l(\eqn{\lambda}) Values
#'
#' @param x a column of data
#' @param lambda a vector of values for the Box-Cox power transformation
#'
#' @return l_lambda, a vector of the same length as lambda for the corresponding l(\eqn{\lambda}) values
#' @export
#'
#' @examples
#' x = 1:10
#' lambda_vector <- seq(0.001, 1, by = 0.001)
#' l_lamb <- l_lambda(x, lambda_vector)
l_lambda <-function(x, lambda) { #x is a column of data, lambda is a vector of the power values

  l_lambda = rep(0, length(lambda)) # create empty vector
  for (i in 1:length(lambda)) {
    x_lamb <- box_cox(x = x, lambda = lambda[i])
    xBar_lamb <- mean(x_lamb)
    n <- length(x_lamb)
    l_lambda[i] <- (-n/2) * log( (1/n) * sum((x_lamb - xBar_lamb)^2)) + (lambda[i] - 1)*sum(log(x))
  }

  return(l_lambda)
}
