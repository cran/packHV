\name{hist_boxplot}
\alias{hist_boxplot}
\title{Plot a histogram with a boxplot below}
\description{Plots a histogram with a boxplot below}
\usage{
hist_boxplot(x, freq = TRUE, density = FALSE, main = NULL, xlab = NULL, ...)
}

\arguments{
  \item{x}{a numeric vector}
  \item{freq}{boolean, \code{TRUE} for frequency or \code{FALSE} probability on the y axis}
  \item{density}{boolean, \code{TRUE} to plot the estimated density}
  \item{main}{character string, main title of the histogram}
  \item{xlab}{character string, label of the x axis}
  \item{\dots}{other arguments to be passed in \code{hist()}}
}

\value{None}
\author{Hugo Varet}

\examples{
par(mfrow=c(1,2))
hist_boxplot(rnorm(100),col="lightblue",freq=TRUE)
hist_boxplot(rnorm(100),col="lightblue",freq=FALSE,density=TRUE)
}
