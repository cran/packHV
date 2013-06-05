\name{cut_quanti}
\alias{cut_quanti}

\title{Cut a quantitative variable in \eqn{n} equal parts}

\description{Cuts a quantitative variable in \eqn{n} equal parts}

\usage{cut_quanti(x, n, ...)}

\arguments{
  \item{x}{a numeric vector}
  \item{n}{numeric, the number of parts: 2 to cut according to the median, and so on...}
  \item{\dots}{other arguments to be passed in \code{\link{cut}}}
}

\value{A factor vector}

\author{Hugo Varet}

\seealso{\code{\link{cut}}}

\examples{
cut_quanti(cgd$height, 3)
}
