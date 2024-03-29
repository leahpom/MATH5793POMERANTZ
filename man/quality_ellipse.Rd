% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/quality_ellipse.R
\name{quality_ellipse}
\alias{quality_ellipse}
\title{Quality Control Ellipse}
\usage{
quality_ellipse(df, var1, var2, alpha = 0.05)
}
\arguments{
\item{df}{a multivariate data.frame}

\item{var1}{the column number of the first variable for the ellipse}

\item{var2}{the column number of the second varaible for the ellipse}

\item{alpha}{the signficance level, defaulted to 0.05}
}
\value{
g a quality control ellipse
}
\description{
Quality Control Ellipse
}
\examples{
weld <- data.frame(weld)
weldln <- data.frame(cbind(weld[,1:3], log(weld[,4])))
colnames(weldln) <- c("Voltage", "Current", "FeedSpeed", "ln(GasFlow)")
quality_ellipse(weldln, 1, 4, alpha = 0.01)
}
