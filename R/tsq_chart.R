#' @title \eqn{T^2} Chart
#'
#' @param df a numeric multivariate data.frame
#'
#' @return p a \eqn{T^2} chart for the data frame with a dashed line at the 95% limit and a solid line at the 99% limit
#' @export
#'
#' @examples
#' tsq_chart(datasets::mtcars)
tsq_chart <- function(df){
  # df is a multivariate data.frame
  # alpha is the significance level

  dat <- as.matrix(df) # turn into a matrix for notation
  xBar <- colMeans(dat)
  S <- cov(dat)
  Sinv <- solve(S)
  T.sq <- matrix(rep(0, nrow(dat)), nrow = nrow(dat), ncol = 1) # empty matrix to store T^2 values
  # create the T.sq values and store
  for (i in 1:nrow(dat)) {
    obs <- dat[i, ]
    val <- t(obs - xBar) %*% Sinv %*% (obs - xBar)
    T.sq[i,] <- val
  }

  # now create a vector of the observation number (period)
  period <- 1:nrow(dat)

  # create data frame for graphing
  df2 <- data.frame(cbind(T.sq, period))
  colnames(df2) <- c("T.sq", "period")

  # figure out where the dashed line should be based on user input
  dl <- stats::qchisq(p = 0.05, df = ncol(dat), lower.tail = FALSE)
  dl2 <- stats::qchisq(p = 0.01, df = ncol(dat), lower.tail = FALSE)

  # graph
  p <- ggplot2::ggplot(data = df2, ggplot2::aes(period, T.sq)) +
    ggplot2::geom_point() + ggplot2::geom_hline(yintercept = dl, linetype = "dashed") +
    ggplot2::geom_hline(yintercept = dl2) +
    ggplot2::geom_text(x = 5, y = dl + 0.5, label = "95% Limit") +
    ggplot2::geom_text(x = 5, y = dl2 + 0.5, label = "99% Limit")

  return(p)

}
