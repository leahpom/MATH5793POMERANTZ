#' @title Box-Cox Transformation
#'
#' @description Returns the transformed values of a particular column of data using the Box-Cox method for a
#' particular value of lambda. The transformation is \eqn{\frac{x^{\lambda} - 1}{x}} if \eqn{\lambda ≠ 0} and
#' \eqn{log_e(x)} if \eqn{\lambda = 0}
#'
#' @param x a vector of data
#' @param lambda the value of lambda for the Box-Cox transformation
#'
#' @return x_lamb the transformed data
#' @export
#'
#' @examples
#' box_cox(x = 1:10, lambda = 0)
box_cox <- function(x, lambda){ # x is a column of data, lambda is the value for the box-cox transformation
  if(lambda == 0){
    x_lamb <- log(x) # create transformed vector
  }
  else if(lambda != 0){
    x_lamb <- ((x^lambda) - 1) / lambda
  }

  return(x_lamb)
}
