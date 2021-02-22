#' @title Rotations and Data Plots
#'
#'
#'
#' @description User uploads any multivariate data set with at least two continuous variables and it \emph{must}
#' be a .csv file. User then specifies which columns of data they are interested in, and a table and two scatterplots
#' are created. The table is the first six entries of the data set, for user reference. The first scatterplot
#' allows the user to create a aesthic graph and look at the drop-one correlation. The second allows the user
#' to rotate the axes by theta (from user input) and gives the correlation corresponding to that theta and the
#' theta at which the correlation will be zero.
#'
#' @return  Table with the first six entries of the uploaded data
#' @return  A scatterplot that is a ggplot2 object
#' @return  A second ggplot2 scatterplot
#'
#' @section shiny:
#' The shiny web server will run once this function is invoked and will open a web browser. You should learn how this is implemented.
#'
#' The web server can be studied \url{https://shiny.rstudio.com/tutorial/}
#'
#' @section First Plot:
#' This scatterplot responds to user input to do the following things:
#'
#' \enumerate{
#' \item The user uploads the data set
#' \item The user selects their variables of interest by inputing the column number - default columns 1 and 2
#' \item The user sets the color of the scatterplot using hexadecimal color codes - default to black
#' \item The user sets the size of the points using a slider - default to 2.5
#' \item The user can update the labels of the axes - default to xLab and yLab
#' \item The user can click on a point on the graph and it will return the drop-1 correlation (correlation \emph{without} the clicked point)
#' }
#'
#' @section Second Plot:
#' This scatterplot also uses the uploaded data and variables of interest and responds to user input to do the following things:
#'
#' \enumerate{
#' \item The user selects a theta value that is then used to rotate the axes by theta degrees
#' \item The selected theta value is also used to calculate the sample correlation
#' \item The user also selects how long the rotated axes are
#' \item The sample correlation and the first-quadrant solution (calculated using the uniroot() function) are printed
#' }
#'
#' @export
#'
#' @examples
#' \dontrun{ shiny_rot()}
shiny_rot<-function(){
  shiny::runApp(system.file("shiny_exercise", package="MATH5793POMERANTZ"),launch.browser = TRUE)
}
