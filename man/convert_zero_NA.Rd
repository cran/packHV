\name{convert_zero_NA}
\alias{convert_zero_NA}
\title{Convert 0s in NA}
\description{Converts 0s in NA}

\usage{convert_zero_NA(data, vars)}

\arguments{
  \item{data}{data frame in which we can find \code{vars}}
  \item{vars}{vector of character string of covariates}
}

\value{The modified data frame}

\author{Hugo Varet}

\examples{
my.data=data.frame(x=rbinom(20,1,0.5),y=rbinom(20,1,0.5),z=rbinom(20,1,0.5))
my.data=convert_zero_NA(my.data,c("y","z"))
}
