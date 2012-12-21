\name{multi.table}
\alias{multi.table}
\title{Multi cross table}
\description{Builds a big cross table between several covariates}
\usage{multi.table(data, vars)}
\arguments{
  \item{data}{data frame in which we can find \code{vars}}
  \item{vars}{vector of character string of covariates}
}
\value{None}
\author{Hugo Varet}

\seealso{\code{\link{plot_multi.table}}}

\examples{
N=100
my.data=data.frame(x=factor(sample(0:2,N,TRUE)),
                   y=factor(sample(0:4,N,TRUE)),
                   z=factor(sample(0:3,N,TRUE)))
multi.table(my.data,c("x","y","z"))
}