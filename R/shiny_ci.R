#' @title Shiny App for Simultaneous Confidence Intervals of \eqn{\mu}
#'
#' @description The user can upload any multivariate data set with at least two continuous variables and it \emph{must}
#' be a .csv file. The user then specifies which columns of data they're interested in examining, which produces several
#' tables and graphs. The first table is just the first six entries of data, for the user to check that they have
#' the right data. There is then a graph of the (1-\eqn{\alpha})*100\% confidence ellipse for the columns of data, with
#' dashed lines drawing the \eqn{\T[2]} intervals of the specified columns. Below that graph is a table that outputs
#' the (1-\eqn{\alpha})*100\% \eqn{\T[2]} confidence intervals for all columns of the data. Next is a graph of the
#' (1-\eqn{\alpha})*100\% confidence ellipse with dashed lines drawing the Bonferroni confidence intervals for the
#' specified columns of data. The last graph is the same confidence ellipse, but with both the \eqn{T[2]} and the
#' Bonferroni confidence intervals drawn on it, for comparison. The last output is a table with the \eqn{T[2]}
#' confidence intervals for the difference in means of the specified columns (\eqn{\mu_i - \mu_k}).
#'
#' @return a table with the first six entries of the user-uploaded data
#' @return a graph with the (1-\eqn{\alpha})*100\% confidence ellipse for the columns of data, with dashed lines drawing the \eqn{\T[2]} intervals
#' @return a table with the (1-\eqn{\alpha})*100\% \eqn{T[2]} confidence intervals for all the columns of data
#' @return a graph with the (1-\eqn{\alpha})*100\% confidence ellipse with dashed lines drawing the Bonferroni confidence intervals
#' @return a table with the (1-\eqn{\alpha})*100\% Bonferroni confidence intervals for all the columns of data
#' @return a graph with the  (1-\eqn{\alpha})*100\% confidence ellipse for the columns of data, with dashed lines drawing the Bonferroni intervals and dotted lines drawing the \eqn{\T[2]} intervals
#'
#' @section User Input:
#'
#' The user has the following input options:
#'
#' \enumerate{
#' \item The user inputs a .csv file
#' \item The user enters the column number of the first variable for the confidence ellipses
#' \item The user enters the column number of the first variable for the confidence ellipses
#' \item The user enters the \eqn{\alpha} value for the confidence ellipses
#' \item The user enters a label for the y-axis of the graphs
#' \item The user enters a label for the x-axis of the graphs
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{ shiny_ci()}
shiny_ci<-function(){
  shiny::runApp(system.file("Project3", package="MATH5793POMERANTZ"),launch.browser = TRUE)
}
