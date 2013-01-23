\name{convert_factor}
\alias{convert_factor}
\title{Convert variables of a data frame in factors}

\description{Converts variables of a data frame in factors}

\usage{convert_factor(data, vars)}

\arguments{
  \item{data}{data frame in which we can find \code{vars}}
  \item{vars}{vector of character string of covariates}
}

\value{The modified data frame}

\author{Hugo Varet}

\examples{
my.data=data=data.frame(x=rbinom(20,1,0.5),y=rbinom(20,1,0.5),z=rbinom(20,1,0.5))
my.data=convert_factor(my.data,c("x","y"))
}
