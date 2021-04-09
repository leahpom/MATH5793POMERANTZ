#' @title Xbar Charts
#'
#' @param x a column of data
#'
#' @return a graph showing the univariate X-bar chart for x
#' @export
#'
#' @importFrom graphics abline
#' @importFrom stats sd
#'
#' @examples
#' weldln <- data.frame(cbind(weld[,1:3], log(weld[,4])))
#' colnames(weldln) <- c("Voltage", "Current", "FeedSpeed", "ln(GasFlow)")
#' y <- weldln$`ln(GasFlow)`
#' XbarCharts(y)
XbarCharts <- function(x){ # x is a column of data
  momts <- list(mean(x), stats::sd(x))
  plot(x,
       type = "b",
       ylim = c(momts[[1]]-4*momts[[2]],momts[[1]]+4*momts[[2]] ),
       xlab = "Observation Number",
       ylab = "Individual Value"
  )
  L = momts[[1]]-3*momts[[2]]
  U = momts[[1]]+ 3*momts[[2]]
  graphics::abline(h = c(momts[[1]], L,U))
}
