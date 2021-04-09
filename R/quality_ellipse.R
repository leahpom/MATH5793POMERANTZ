#' @title Quality Control Ellipse
#'
#' @param df a multivariate data.frame
#' @param var1 the column number of the first variable for the ellipse
#' @param var2 the column number of the second varaible for the ellipse
#' @param alpha the signficance level, defaulted to 0.05
#'
#' @return g a quality control ellipse
#' @export
#'
#' @examples
#' weld <- data.frame(weld)
#' weldln <- data.frame(cbind(weld[,1:3], log(weld[,4])))
#' colnames(weldln) <- c("Voltage", "Current", "FeedSpeed", "ln(GasFlow)")
#' quality_ellipse(weldln, 1, 4, alpha = 0.01)
quality_ellipse <- function(df, var1, var2, alpha = 0.05){
  df <- data.frame(cbind(df[,var1]), df[,var2])
  xBar1 <- colMeans(df)
  S <- cov(df)
  x0 <- xBar1[1]
  y0 <- xBar1[2]
  eigenS <- eigen(S)
  lambda1 <- eigenS$values[1]
  lambda2 <- eigenS$values[2]
  e1 <- eigenS$vectors[,1]
  e2 <- eigenS$vectors[,2]
  cSq <- qchisq(alpha, 2, lower.tail = FALSE) # chi-square value
  axis1 <- sqrt(cSq) * sqrt(lambda1)
  axis2 <- sqrt(cSq) * sqrt(lambda2)
  theta <- acos(eigenS$vectors[1,1])

  g <- ggplot2::ggplot(data = df, ggplot2::aes(x = df[,1], y = df[,2])) +
    ggplot2::geom_point() + ggplot2::xlab("x1") + ggplot2::ylab("x2") +
    ggforce::geom_ellipse(ggplot2::aes(x0 = x0, y0 = y0, a = axis1, b = axis2, angle = theta))

  return(g)

}
