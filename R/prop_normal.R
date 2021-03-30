#' Normalcy Proportions Test
#'
#' @description Takes a vector x, from a data frame, and checks what proportion is within one and two standard deviations of the mean
#'
#' @param x a vector
#'
#' @return an invisible list with two String variables stating whether or not the data column passes the check and the values of the upper and lower bounds of the intervals
#' @export
#'
#' @importFrom stats var
#'
#' @examples
#' \dontrun{
#' x <- diamonds[1]
#' prop_normal(x)
#' }
prop_normal <- function(x){ # x is a column
  x <- as.matrix(x)
  n <- nrow(x) # calculate the number of times to run the loop
  xBar <- colMeans(x) # calculate each x-bar_i
  s <- var(x) # calculate the variance

  # for one standard deviation
  intervalLB = xBar - sqrt(s) # lower-bound
  intervalUB = xBar + sqrt(s)  # upper-bound
  count1 = 0 # initialize to zero
  for (i in 1:n){
    if(dplyr::between(x[i,1], intervalLB, intervalUB)){
      count1 = count1 + 1 # increment count to keep track of how many are in the interval
    }
  }

  # for two standard deviations
  intervalLB2 = xBar - 2*sqrt(s) # lower-bound
  intervalUB2 = xBar + 2*sqrt(s)  # upper-bound
  count2 = 0 # initialize to zero
  for (i in 1:n){
    if(dplyr::between(x[i,1], intervalLB2, intervalUB2)){
      count2 = count2 + 1 # increment count to keep track of how many are in the interval
    }
  }

  prop1 <- count1 / n

  prop1Adj <- prop1 - 0.683
  compVal <- 1.396/sqrt(n) # value to compare to, from textbook

  if(abs(prop1Adj) > compVal){
    pass1 <- "Failed normalcy check based on proportion 1 standard deviation from the mean"
  }
  else{
    pass1 <- "Passed normalcy check based on proportion 1 standard deviation from the mean"
  }

  prop2 <- count2 / n

  prop2Adj <- prop2 - 0.954
  compVal2 <- 0.628/sqrt(n) # value to compare to, from textbook

  if(abs(prop2Adj) > compVal2){
    pass2 <- "Failed normalcy check based on proportion 2 standard deviations from the mean"
  }
  else{
    pass2 <- "Passed normalcy check based on proportion 2 standard deviations from the mean"
  }

  endList <- list(pass1 = pass1, pass2 = pass2, lower.bound1 = intervalLB,
                  upper.bound1 = intervalUB, lower.bound2 = intervalLB2, upper.bound2 = intervalUB2)
  return(invisible(endList))

  # commented out to check returning an invisible list
  # endList <- list(pass1 = pass1, pass2 = pass2)
  # return(endList)

}
