#' @title PCA and Bootstrapping
#'
#' @description This is a function for the PCA bootstrapping assignment. It performs bootstrapping on the
#' eigenvalues and eigenvectors of either the sample covariance (S) or the sample correlation (R), then it
#' outputs plots of the bootstrapped estimates, as well as confidence intervals. It also outputs Anderson and
#' Girschick confidence intervals for the \eqn{\lambda} values
#'
#' @param data a data frame
#' @param alpha the significance level
#' @param iter the number of iterations to use when bootstrapping
#' @param cov a boolean for if we want to use S (cov = TRUE) or R (cov = FALSE) for the analysis
#'
#' @return a barplot for the bootstrapped values of each \eqn{\lambda[i]}
#' @return a violin plot for the bootstrapped values for each e_ik, i,k = 1,2,...p, where p is the number of variables
#' @return a list containing confidence intervals for each \eqn{\lambda[i]} and e_ik, i,k = 1,2,...p from bootstrapping, and confidence intervals for each \eqn{\lambda[i]} using the Anderson and Gerschick method
#'
#'
#' @export
#'
#' @examples
#' data <- as.data.frame(datasets::mtcars)
#' pcabootAG(data[,1:3], alpha = 0.05, iter = 100)
#'
#'
pcabootAG <- function(data, alpha, iter, cov = TRUE){

  val <- NULL

  # make statistics for lambda and e
  # make functions to use in the boot function for each
  myfunlamb <- function(data, indices){
    # set value for analysis (S or R)
    if (cov == TRUE){
      bootVal = cov(data[indices,])# this will use S for the analysis
    }
    else if (cov == FALSE){
      bootVal = cor(data[indices,]) # this will use R for the analysis
    }# function to use for bootstrapping the lambda values
    E <- eigen(bootVal)

    lambda <-E$values
    return(c(lambda))
  }

  # function for eigenvectors
  myfunvector <- function(data, indices){
    # set value for analysis (S or R)
    if (cov == TRUE){
      bootVal = cov(data[indices,])# this will use S for the analysis
    }
    else if (cov == FALSE){
      bootVal = cor(data[indices,]) # this will use R for the analysis
    }# function to use for bootstrapping the lambda values
    E <- eigen(bootVal)

    eVector <-E$vectors
    return(c(eVector))
  }


  # create lambda estimates
  lambdaEst <- boot::boot(data,myfunlamb, R = iter)

  # create eigenvector estimates
  eVectorEst <- boot::boot(data,myfunvector, R = iter)
  #return(eVectorEst) # FIX ME: examining the output


  # plot histograms of the bootstrap estimates for the eigenvalues - use the par function and a for-loop
  nGraphs = ncol(data) # this sets the number we need for our iterations and for our par
  par(mfrow=c(nGraphs,1)) # create the par before the loop so we can fill it in

  for (i in 1:nGraphs) {
    h <- graphics::hist(lambdaEst$t[,i], plot = FALSE)
    d <- h$density
    den <- d/max(d)
    graphics::hist(lambdaEst$t[,i], freq = FALSE,
         col = grDevices::rgb(0,den,1-den^2, 0.7),
         main = bquote("Bootstrap distribution of " ~ widehat(lambda)[.(i)]),
         xlab = bquote(widehat(lambda)[.(i)]))
  }

  # plot violin plots of the estimates for the eignevalues - use the par function and a for-loop again
  # we want p graphs, each with p "violins"
  #par(mfrow=c(nGraphs,1)) # create the par before the loop so we can fill it in - couldn't get to work :(
  for (index in 1:nGraphs) {
    # we need to create a category variable for the x-axis
    #cat <- rep(c((nGraphs*(i-1)+1):(nGraphs*i)), iter*(c(1,1,1)))
    cat <- rep(c(1:nGraphs), iter*(c(1,1,1)))
    cat <- matrix(cat,nr = nGraphs*iter, nc = 1, byrow = FALSE)
    cat <- as.factor(cat)

    # vec is an p*iter by 1 matrix, and I want to plot the iter values of each e[ngraph, ngraph], so I need to turn it
    # add the cat factor to do that
    vec <- matrix(c(eVectorEst$t[,(nGraphs*(index-1)+1):(nGraphs*index)]), nrow = nGraphs*iter, ncol = 1)

    graphDF <- as.data.frame(cbind(cat, vec)) # bind the columns together
    colnames(graphDF) <- c("cat", "val")
    graphDF$cat <- as.factor(graphDF$cat) # I want to be really really sure it's a factor

    # create plot
    g <- ggplot2::ggplot(graphDF, ggplot2::aes(x = cat, y = val, color = cat)) +
      ggplot2::geom_violin() +
      ggplot2::scale_color_manual(values=c("#FFC93C", "#9DDFD3", "#31326F")) +
      ggplot2::ggtitle(bquote("Plot of the iter values for each value in" ~ widehat(e)[.(index)])) +
      ggplot2::xlab(bquote(widehat(e)[.(index)])) +
      ggplot2::labs(color = (bquote(widehat(e)[.(index)])))
    print(g)

  }

  # bootstrap confidence intervals for lambda

  # do as a for-loop for each lambda
  ci <- rep(0, 2*nGraphs)
  ci <- matrix(ci, nrow = nGraphs, ncol = 2)  # empty matrix to store each interval
  lb <- as.character(100*(alpha/2))
  ub <- as.character(100*(1-alpha/2))
  colnames(ci) <- c(paste(lb, "%"), paste(ub, "%"))
  for( i in 1:nGraphs){
    ci[i,] <- stats::quantile(lambdaEst$t[,i], c(alpha/2, 1-alpha/2))
  }

  # bootstrap confidence intervals for eigenvectors
  ci2 <- rep(0, 2*nGraphs*nGraphs)
  ci2 <- matrix(ci, nrow = nGraphs*nGraphs, ncol = 2) # empty matrix to store values

  # column labels to make it look nice
  lb <- as.character(100*(alpha/2))
  ub <- as.character(100*(1-alpha/2))
  colnames(ci2) <- c(paste(lb, "%"), paste(ub, "%"))

  # row labels to make it look nice
  r1 <- rep(c(1:nGraphs), c(nGraphs, nGraphs, nGraphs))
  r2 <- rep(c(1:nGraphs), nGraphs)
  rownames(ci2) <- c(matrix(c(paste("e_", r1, r2)), nrow = nGraphs*nGraphs, ncol = 1))

  # calculate the values
  for( i in 1:nGraphs){
    ci2[i,] <- stats::quantile(eVectorEst$t[,i], c(alpha/2, 1-alpha/2))
  }

  # create A and G interval function for the lambdas
  andgirL <- function(lambda,n, alpha = 0.05){
    ci <- vector("numeric", length = 2)
    ci[1]<- lambda/(1+qnorm(1-alpha/2)*sqrt(2/n))
    ci[2]<- lambda/(1-qnorm(1-alpha/2)*sqrt(2/n))
    ci
  }

  # create the A and G intervals for the lambdas by running the function in a for-loop
  if (cov == TRUE){
    bootVal = cov(data)# this will use S for the analysis
  }
  else if (cov == FALSE){
    bootVal = cor(data) # this will use R for the analysis
  }
  E <- eigen(bootVal)
  ciL <- rep(0, 2*nGraphs)
  ciL <- matrix(ci, nrow = nGraphs, ncol = 2)  # empty matrix to store each interval
  colnames(ciL) <- c(paste(lb, "%"), paste(ub, "%")) # row names to make the output easier to read
  for (iL in 1:nGraphs){
    ciL[iL,] <- andgirL(E$values[iL], n = nrow(data), alpha = alpha)
  }



  # FINAL: list and return statement
  finalList <- list("bootCI.lambda" = ci,
                    "bootCI.eigen" = ci2,
                    "AGci.lambda" = ciL)

  return(finalList)


}
