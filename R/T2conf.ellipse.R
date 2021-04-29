#' @title (1 - \eqn{\alpha})*100\% Confidence Ellipses
#'
#' @param data a two-vector data.frame
#' @param alpha the significance level, set to the default of 0.05
#' @param col1 an integer saying which is the first column the user would like to look at
#' @param col2 an integer saying which is the second column the user would like to look at
#'
#' @description I used Figures 5.1 and 5.2 o pages 23 and 227, respectively, of our
#' book, \emph{Applied Multivariate Statistical Analysis}, 6th Edition, by Johnson and Wichern, to create the confidence
#' ellipse. This is similar to the funciton I made called conf.ellipse, but this one \emph{only} outputs the graph, it
#' outputs it as an object (so I can use it in my Shiny app), and it adds the dashed lines
#'
#' @return g a ggplot2 object with a drawing of the confidence ellipse (solid) with the T^2 intervals drawn on with dashed lines
#' @export
#'
#' @examples
#' testData <- datasets::mtcars[, 1:2]
#' T2conf.ellipse(testData, 0.05)
T2conf.ellipse <- function(data, alpha = 0.05, col1 = 1, col2 = 2){ # data is a two-vector data frame
  # 1 - alpha is the confidence level
  quad.form <- NULL # attempt to fix error of "no visible binding for global variable" from check

  # Check before the function executes - make sure everything is in the right form

  # a check that alpha has the appropriate values
  if(abs(alpha) > 1){
    stop("alpha must be a scalar quantity between 0 and 1")
  }

  # a check that there are two columns to look at
  if(col1 == col2){
    stop("This is a bivariate check, there must be two columns - adjust values in col1 and col2")
  }

  data <- data[,c(col1, col2)] # reduce the data to only the observations we are looking at

  n <- nrow(data) # number of observations, n
  P <- 2 # the number of variables; a constant, 2, since this is for a bivariate data set

  xBar <- matrix(colMeans(data), nrow = 2, ncol = 1) # make sure our x-bar is in the right form

  S <- cov(data) # calculate the S
  Sinv <- solve(S) # S^(-1)

  # Calculate the critical value
  Fp <- stats::qf(p = alpha, df1 = P, df2 = n - P, lower.tail = FALSE) # calculate the relevant F-value
  critVal <- (P*(n-1))/(n-P) * Fp # calculate the critical value for comparison

  # Do the ellipse parts

  # major/minor half axes

  eigenVVS <- eigen(S)

  lambda1 <- eigenVVS$values[1] # lambda1, for easy reference
  lambda2 <- eigenVVS$values[2] # lambda2, for easy reference

  axis1 <- sqrt(lambda1) * sqrt( (P*(n-1))/(n*(n-P)) * Fp) # first axis
  axis2 <- sqrt(lambda2) * sqrt( (P*(n-1))/(n*(n-P)) * Fp) # second axis

  # Create the plot
  theta <- acos(eigenVVS$vectors[1,1])

  T2 <- ci_T2(data = data, alpha = alpha)

  g <- ggplot2::ggplot(data = data, ggplot2::aes(x = data[,1], y = data[,2])) +
    ggplot2::xlab("mu_1") + ggplot2::ylab("mu_2") +
    ggplot2::ggtitle(bquote(.((1-alpha)*100) ~ "% " ~ T^2 ~ "Confidence Ellipse")) +
    ggforce::geom_ellipse(ggplot2::aes(x0 = xBar[1], y0 = xBar[2], a = axis1, b = axis2, angle = theta)) +
    ggplot2::geom_segment(ggplot2::aes(x = T2[1,1], y = T2[2,1], xend = T2[1,1], yend = T2[2,2]), linetype = "dashed") +
    ggplot2::geom_segment(ggplot2::aes(x = T2[1,2], y = T2[2,1], xend = T2[1,2], yend = T2[2,2]), linetype = "dashed") +
    ggplot2::geom_segment(ggplot2::aes(x = T2[1,1], y = T2[2,1], xend = T2[1,2], yend = T2[2,1]), linetype = "dashed") +
    ggplot2::geom_segment(ggplot2::aes(x = T2[1,1], y = T2[2,2], xend = T2[1,2], yend = T2[2,2]), linetype = "dashed")

  # return statement
  return(g)

}
