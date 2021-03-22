#' @title Confidence regions for bivariate data
#'
#' @param data a data.frame containing two variables
#' @param alpha a scaled quantity between 0 and 1; 100(1-alpha) is the confidence level
#' @param mu_0 the vector of means from the null hypothesis
#'
#' @return a plot showing the ellipse that is the confidence region based on alpha
#' @return a named list with several outputs, explained further in the output section
#'
#' @section List of output:
#'
#' The list returned at the end of the function has several components to its output:
#'
#' \enumerate{
#' \item The result of the confidence test (result)
#' \item The size of the quadratic form (quadratic.form)
#' \item The scaled quantile (scaled.quantile)
#' \item The eigenvectors and eigenvalues of S (eigenvalues and eigenvectors)
#' \item The major and minor half-widths (major.halfwidth and minor.halfwidth)
#' \item The lengths ratio, i.e. ratio of the major axis to the minor axis (lengths.ratio)
#' }
#'
#' @importFrom graphics mtext points segments title par
#' @importFrom stats cor cov qf
#'
#' @export
#'
#' @examples
#' \dontrun{
#' df <- diamonds[, c(1,5)]
#' dBar <- colMeans(data)
#' conf.ellipse(data = df, alpha = 0.05, mu_0 = dBar)}
conf.ellipse <- function(data, alpha, mu_0){ # data is a two-vector data frame
  # 1 - alpha is the confidence level
  # mu_0 is the vector of means for the null hypothesis

  # Check before the function executes - make sure everything is in the right form

  # a check that alpha has the appropriate values
  if(abs(alpha) > 1){
    stop("alpha must be a scalar quantity between 0 and 1")
  }

  n <- nrow(data) # number of observations, n
  P <- 2 # the number of variables; a constant, 2, since this is for a bivariate data set

  xBar <- matrix(colMeans(data), nrow = 2, ncol = 1) # make sure our x-bar is in the right form

  mu_0 <- matrix(mu_0, ncol = 1) # make sure our mu_0 is in the right form

  S <- cov(data) # calculate the S
  Sinv <- solve(S) # S^(-1)

  xBarMu <- xBar - mu_0 # important for the full formula and easier to do in pieces to avoid a mistake

  quad.Form <- n * t(xBarMu) %*% Sinv %*% xBarMu

  # Calculate the critical value
  Fp <- qf(p = 1 - alpha, df1 = P, df2 = n - P) # calculate the relevant F-value
  critVal <- (P*(n-1))/(n-P) * Fp # calculate the critical value for comparison

  # Calculate the result of the test
  if(quad.Form <= critVal){
    resultTest <- "Fail to reject the null hypothesis at the given significance level; mu_0 is in the confidence region"
  }
  else if (quad.form > critVal) {
    resultTest <- "Reject the null hypothesis at the given significance level; mu_0 is not in the confidence region"
  }


  # Do the ellipse parts

  # Center of the ellipse
  centerEllipse <- t(xBar)

  # major/minor half axes

  eigenVVS <- eigen(S) # NOTE: also should be in list output

  lambda1 <- eigenVVS$values[1] # lambda1, for easy reference
  lambda2 <- eigenVVS$values[2] # lambda2, for easy reference

  axis1 <- sqrt(lambda1) * sqrt( (P*(n-1))/(n*(n-P)) * Fp) # first axis
  axis2 <- sqrt(lambda2) * sqrt( (P*(n-1))/(n*(n-P)) * Fp) # second axis

  # ratio

  ratio = sqrt(lambda1)/sqrt(lambda2)

  # Create the plot
  r <- cor(data[,1], data[,2]) # calculate the correlation
  theta <- acos(r) # use it to find the rotation of the ellipse

  # write the plot in base R
  plot(1, type = "n", xlab = "", ylab = "",
       xlim = c(xBar[1,] - 3*axis1, xBar[1,] + 3*axis1),
       ylim = c(xBar[2,] - 3*axis2, xBar[2,] + 3*axis2))
  # draw the ellipse
  DescTools::DrawEllipse(x = xBar[1,], y = xBar[2,], radius.x = axis1,
                         radius.y = axis2,
                         rot = theta,nv = 100, border = par("fg"), col = par("bg"),
                         lty = par("lty"), lwd = par("lwd"), plot = TRUE)

  segments(x0 = c(xBar[1,], 0), y0 = c(0, xBar[2,]), x = c(xBar[1,], xBar[1,]),
           y = c(xBar[2,], xBar[2,]), col = "red", lty = 2) # draw dashed lines to the center
  # draw a point at the center
  points(x = xBar[1,], y = xBar[2,], col = "red", lwd = 3, pch = 19)
  mtext(text = "x-bar1", side = 1) # write labels for the center
  mtext(text = "x-bar2", side = 2)
  # give it a helpful title
  title(main = "Confidence Region for mu",
        xlab = "x1",
        ylab = "x2")


  # now release the named list of all the output
  list.check <- list(result = resultTest, quadratic.form = quad.Form,
                     scaled.quantile = critVal, eigenvalues = eigenVVS$values,
                     eigenvectors = eigenVVS$vectors, major.halfwidth = axis1,
                     minor.halfwidth = axis2, lengths.ratio = ratio)
  return(list.check)

}
