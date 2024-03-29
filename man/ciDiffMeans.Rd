% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ciDiffMeans.R
\name{ciDiffMeans}
\alias{ciDiffMeans}
\title{\eqn{T^2} Confidence Interval for the Difference in Means}
\usage{
ciDiffMeans(data, alpha = 0.05, i = 1, k = 2)
}
\arguments{
\item{data}{a data.frame}

\item{alpha}{the significance level, set to the standard 0.05}

\item{i}{the first column in our data frame that we want to use in our calculation}

\item{k}{the second column in our data frame that we want to use in our calculation}
}
\value{
The \eqn{(1 - \alpha)*100}\% confidence interval for the difference in means, \eqn{\mu_i - \mu_k}
}
\description{
We use equation 5-25, found on page 226 of our book, \emph{Applied Multivariate Statistical Analysis},
 6th Edition, by Johnson and Wichern
}
\examples{
testData <- datasets::mtcars
ciDiffMeans(data = testData, i = 1, k = 2)

}
