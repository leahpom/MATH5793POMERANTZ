% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prop_normal.R
\name{prop_normal}
\alias{prop_normal}
\title{Normalcy Proportions Test}
\usage{
prop_normal(x)
}
\arguments{
\item{x}{a vector}
}
\value{
an invisible list with two String variables stating whether or not the data column passes the check and the values of the upper and lower bounds of the intervals
}
\description{
Takes a vector x, from a data frame, and checks what proportion is within one and two standard deviations of the mean
}
\examples{
\dontrun{
x <- diamonds[1]
prop_normal(x)
}
}
