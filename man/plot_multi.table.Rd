\name{plot_multi.table}
\alias{plot_multi.table}
\title{Plot a multi cross table}
\description{
Plots a multi cross table on a graph
}
\usage{
plot_multi.table(data, vars)
}
\arguments{
  \item{data}{data frame in which we can find \code{vars}}
  \item{vars}{vector of character string of covariates}
}
\value{None}
\author{Hugo Varet}


\seealso{\code{\link{multi.table}}}

\examples{
N=100
my.data=data.frame(x=factor(sample(0:2,N,TRUE)),
                   y=factor(sample(0:4,N,TRUE)),
                   z=factor(sample(0:3,N,TRUE)))
plot_multi.table(my.data,c("x","y","z"))
}