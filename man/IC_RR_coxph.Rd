\name{IC_RR_coxph}
\alias{IC_RR_coxph}
\title{RR and their confidence intervals for Cox models}
\description{Computess risk ratios and their confidence intervals for Cox models}
\usage{IC_RR_coxph(model, alpha = 0.05, sided = 2)}
\arguments{
  \item{model}{a \code{coxph} object}
  \item{alpha}{type I error, 0.05 by default}
  \item{sided}{1 or 2 for one or two-sided}
}

\value{A matrix with the estimaed coefficients of the Cox model, their s.e., z-values, p-values, RR and CI of the RR}

\author{Hugo Varet}

\examples{
N=100
my.data=data.frame(time=rexp(N),cens=rbinom(N,1,0.7),x1=rnorm(N),x2=rnorm(N))
IC_RR_coxph(coxph(Surv(time,cens)~x1+x2,data=my.data),alpha=0.05,sided=1)
}

