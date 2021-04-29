#' @title Bonferroni Confidence Intervals with \eqn{T[2]} Comparison
#'
#' @param data a two-vector data frame
#' @param alpha the significance level
#' @param col1 an integer saying which is the first column the user would like to look at
#' @param col2 an integer saying which is the second column the user would like to look at
#' @param addT2 a boolean variable. Default is FALSE but, if TRUE, adds \eqn{T[2]} intervals to the graph
#'
#' @return g a ggplot object containing the confidence ellipse with a dashed line showing the Bonferroni intervals and (if addT2 is set to TRUE) a dotted line showing the \eqn{T[2]} intervals
#' @export
#'
#' @examples
#' testData <- datasets::mtcars
#' BFconf.ellipse(testData)
#'
BFconf.ellipse <- function(data, alpha = 0.05, col1 = 1, col2 = 2, addT2 = FALSE){ # data is a two-vector data frame
  # 1 - alpha is the confidence level
  # T2 is a boolean variable and if TRUE adds T^2 intervals in addition to the Bonferroni Intervals
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
  Fp <- qf(p = alpha, df1 = P, df2 = n - P, lower.tail = FALSE) # calculate the relevant F-value
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
  BF <- bfMean_int(data = data, alpha = 0.05)

  g <- ggplot2::ggplot(data = data, ggplot2::aes(x = data[,1], y = data[,2])) +
    ggplot2::xlab("mu_1") + ggplot2::ylab("mu_2") +
    ggplot2::ggtitle(bquote(.((1-alpha)*100) ~ "% " ~ "Bonferroni Confidence Interval")) +
    ggforce::geom_ellipse(ggplot2::aes(x0 = xBar[1], y0 = xBar[2], a = axis1, b = axis2, angle = theta)) +
    ggplot2::geom_segment(ggplot2::aes(x = BF[1,1], y = BF[2,1], xend = BF[1,1], yend = BF[2,2]), linetype = "dashed") +
    ggplot2::geom_segment(ggplot2::aes(x = BF[1,2], y = BF[2,1], xend = BF[1,2], yend = BF[2,2]), linetype = "dashed") +
    ggplot2::geom_segment(ggplot2::aes(x = BF[1,1], y = BF[2,1], xend = BF[1,2], yend = BF[2,1]), linetype = "dashed") +
    ggplot2::geom_segment(ggplot2::aes(x = BF[1,1], y = BF[2,2], xend = BF[1,2], yend = BF[2,2]), linetype = "dashed")

  if(addT2 == TRUE){
    g <- g + ggforce::geom_ellipse(ggplot2::aes(x0 = xBar[1], y0 = xBar[2], a = axis1, b = axis2, angle = theta)) +
      ggplot2::geom_segment(ggplot2::aes(x = T2[1,1], y = T2[2,1], xend = T2[1,1], yend = T2[2,2]), linetype = "dotted") +
      ggplot2::geom_segment(ggplot2::aes(x = T2[1,2], y = T2[2,1], xend = T2[1,2], yend = T2[2,2]), linetype = "dotted") +
      ggplot2::geom_segment(ggplot2::aes(x = T2[1,1], y = T2[2,1], xend = T2[1,2], yend = T2[2,1]), linetype = "dotted") +
      ggplot2::geom_segment(ggplot2::aes(x = T2[1,1], y = T2[2,2], xend = T2[1,2], yend = T2[2,2]), linetype = "dotted") +
      ggplot2::ggtitle(bquote(.((1-alpha)*100) ~ "% " ~ T^2 ~ " and Bonferroni Confidence Interval"),
                       subtitle = bquote("Dashed line - Bonferroni Intervals, Dotted line - " ~ T^2 ~ " intervals")) +
      ggplot2::geom_text(x = T2[1,2], y = T2[2,2], label = "T2") +
      ggplot2::geom_text(x = BF[1,1], y = BF[2,2], label = "BF")
  }

  # return statement
  return(g)

}
