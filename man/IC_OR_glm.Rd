\name{IC_OR_glm}
\alias{IC_OR_glm}
\title{OR and their confidence intervals for logistic regressions}
\description{Computes odd ratios and their confidence intervals for logistic regressions}
\usage{
IC_OR_glm(model, alpha = 0.05)
}
\arguments{
  \item{model}{a \code{glm} object}
  \item{alpha}{type I error, 0.05 by default}
}

\value{A matrix with the estimaed coefficients of the logistic model, their s.e., z-values, p-values, OR and CI of the OR}

\author{Hugo Varet}

\examples{
y=rbinom(1000,1,0.4)
x1=runif(1000)
x2=runif(1000)
IC_OR_glm(glm(y~x1+x2,family="binomial"))
}
