\name{convert_factor}
\alias{convert_factor}
\title{Convert variables of a data frame in factors}

\description{Converts variables of a data frame in factors}

\usage{convert_factor(data, vars)}

\arguments{
  \item{data}{data frame in which we can find \code{vars}}
  \item{vars}{vector of character string of covariates}
}

\value{None}

\author{Hugo Varet}

\examples{
data=data.frame(x=rbinom(20,1,0.5),y=rbinom(20,1,0.5),z=rbinom(20,1,0.5))
convert_factor(data,c("x","y"))
}
