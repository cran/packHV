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
multi.table(cgd,c("treat","sex","inherit"))
}