% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/IC_OR_glm.r
\name{IC_OR_glm}
\alias{IC_OR_glm}
\title{OR and their confidence intervals for logistic regressions}
\usage{
IC_OR_glm(model, alpha = 0.05)
}
\arguments{
\item{model}{a \code{glm} object}

\item{alpha}{type I error, 0.05 by default}
}
\value{
A matrix with the estimaed coefficients of the logistic model, their s.e., z-values, p-values, OR and CI of the OR
}
\description{
Computes odd ratios and their confidence intervals for logistic regressions
}
\examples{
IC_OR_glm(glm(inherit~sex+age,data=cgd,family="binomial"))
}
\author{
Hugo Varet
}
