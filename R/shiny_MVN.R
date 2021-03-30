#' @title Multivariate Normality Checks
#'
#' @description The user uploads a multivariate data set with at least two continuous variables, and it \emph{must}
#' be a .csv. The app displays the first six entries of the data frame in a table so the user can verify they have
#' selected the right dataset. The user then enters the column number that they are interested in for a Normality
#' Proportion Test. The app will then output two plots with an associated statement saying whether the data passed
#' or failed the check.The first plot shows one standard deviation away from the mean, the second plot shows two
#' standard deviations from the mean. The user also selects the column of data they would like to see a QQ-Plot
#' for. The associated p-value from the Shapiro-Wilk test will also be displayed. The next output is a table
#' displaying the r_q values, where r_q is the correlation coefficient for each column q. There is then a graph
#' showing the ellipse containing approximately 1-\eqn{\alpha} of the data, where \eqn{\alpha} is based on user input.
#' We next have a table showing the generalized distance of each observation for the selected column and the critical
#' value and whether the distance is greater than the critical value. The user can select which rows of the table
#' they want to see. There is then a chi-square plot for the selected column, followed by a table showing the
#' standardized z-values for each column. Again, the user can select which rows of the table they would like to see.
#' To evaluate the bivariate normality, the user then selects two columns of data and the app returns a plot
#' showing the scatterplot for the two variables with marginal dotplots. We then have an updated chi-square plot,
#' which shows the colors of the points based on whether or not the generalized distance is in the upper fifth
#' percentile of the chi-square distribution. We then have a plot for the Box-Cox tranformation. The user selects
#' a column of data that they would like to be transformed and the lambda for the transformation. There is then a
#' plot showing the l(\eqn{\lambda}) vs \eqn{\lambda}. This can help the user select which lambda is best for the
#' input. There are then two more plots. The first is a \emph{clickable} QQ-plot of the Box-Cox transformed data.
#' The user can click on a point on the plot and then another plot will appear, showing the QQ-plot for the Box-Cox
#' transformed data, calculated with the clicked point dropped.
#'
#' @return A table showing the first six entries of the uploaded data set
#' @return A dotplot showing the selected column of data, colored by whether or not the observations are within one standard deviation of the mean
#' @return A dotplot showing the selected column of data, colored by whether or not the observations are within two standard deviations of the mean
#' @return A scatterplot that is the QQ-plot for the selected column of data
#' @return A table that contains the correlation coefficient for each column
#' @return A scatterplot showing the ellipse containing approximately 1-\eqn{\alpha} of the data, where \eqn{\alpha} is based on user input.
#' @return A table showing the generalized distances and critical value for the selected column of data, where the displayed rows are from user input
#' @return A chi-square plot for the selected column of data
#' @return A table showing the standardized z-values for each column, where the displayed rows are from user input
#' @return An updated chi-square plot for the selected column of data where points greater than the upper fifth quantile are colored red
#' @return A plot showing the scatterplot and marginal dotplots for the user-selected columns of data
#' @return A plot showing the l(\eqn{\lambda}) vs. \eqn{\lambda} to help the user select a good \eqn{\lambda} value
#' @return A QQ-plot of the Box-Cox transformed data, which is clickable
#' @return A QQ-plot for the Box-Cox transformed data, calculated having dropped the clicked point from the previous graph
#'
#' @section User Input:
#'
#' The user has the following input options:
#'
#' \enumerate{
#' \item The user inputs a .csv file
#' \item The user enters the column number of a variable for a Normal Proportion Check
#' \item The user enters the column number of the variable for QQ-Plot and Shapiro-Wilk test
#' \item The user enters the column number for the first variable for the bivariate normality check
#' \item The user enters the column number for the second variable for the bivariate normality check
#' \item The user enters the \eqn{\alpha} value for the bivariate normality check
#' \item The user enters the consecutive rows of the Generalized Distances table, separated by a comma
#' \item The user enters the consecutive rows of the Standardized Values table, separated by a comma
#' \item The user enters the lambda for Box-Cox Transformation
#' \item The user enters the column number of the variable for the Box-Cox Evaluation
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{ shiny_MVN()}
shiny_MVN<-function(){
  shiny::runApp(system.file("shiny_MVN/shiny_MVN", package="MATH5793POMERANTZ"),launch.browser = TRUE)
}
