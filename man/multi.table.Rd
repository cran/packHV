\name{multi.table}
\alias{multi.table}
\title{Multi cross table}
\usage{
multi.table(data, vars)
}
\arguments{
  \item{data}{the data frame in which we can find
  \code{vars}}

  \item{vars}{vector of character string of covariates}
}
\value{
A matrix containing all the contingency tables between the
covariates
}
\description{
Builds a big cross table between several covariates
}
\examples{
multi.table(cgd,c("treat","sex","inherit"))
}
\author{
Hugo Varet
}
\seealso{
\code{\link{plot_multi.table}}
}

