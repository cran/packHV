\name{plot_multi.table}
\alias{plot_multi.table}
\title{Plot a multi cross table}
\description{
Plots a multi cross table on a graph
}
\usage{
plot_multi.table(data, vars, main = "")
}
\arguments{
  \item{data}{data frame in which we can find \code{vars}}
  \item{vars}{vector of character string of covariates}
  \item{main}{main title of the plot}
}
\value{None}
\author{Hugo Varet}


\seealso{\code{\link{multi.table}}}

\examples{
plot_multi.table(cgd,c("treat","sex","inherit"))
}