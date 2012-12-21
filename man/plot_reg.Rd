\name{plot_reg}
\alias{plot_reg}
\title{Plot points with the corresponding linear regression line}
\description{Plots points with the corresponding linear regression line}
\usage{
plot_reg(x, y, pch = 19, xlab = NULL, ylab = NULL, ...)
}
\arguments{
  \item{x}{numeric vector}
  \item{y}{numeric vector}
  \item{pch}{type of points}
  \item{xlab}{character string, label of the x axis, \code{NULL} by default}
  \item{ylab}{character string, label of the y axis, \code{NULL} by default}
  \item{\dots}{other arguments to be passed in \code{plot()}}
}

\value{None}

\author{Hugo Varet}

\examples{
N=100
x=rnorm(N)
y=2*x+1+rnorm(N,0,0.2)
my.data=data.frame(x,y)
plot_reg(my.data$x,my.data$y)
}
