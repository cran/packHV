\name{plot_km}
\alias{plot_km}
\title{Kaplan-Meier plot with number of subjects at risk below}
\description{
Kaplan-Meier plot with number of subjects at risk below
}
\usage{
plot_km(formula, data, test = TRUE, times.print = NULL, xlab = NULL, ylab = NULL, 
        left = 4.5, bottom = 5, cex.mtext = 1, ...)
}
\arguments{
  \item{formula}{same formula than in \code{survfit()} (\code{Surv(time,cens)~group} or \code{Surv(time,cens)~1})}
  \item{data}{data frame with \code{time}, \code{cens} and \code{group}}
  \item{test}{boolean, \code{TRUE} to compute and display the p-value of the log-rank test}
  \item{times.print}{numeric vector, times at which to display the numbers of subjects at risk}
  \item{xlab}{character string, label of the time axis}
  \item{ylab}{character string, label of the y axis}
  \item{left}{integer, size of left margin}
  \item{bottom}{integer, number of lines in addition of the table below the graph}
  \item{cex.mtext}{numeric, size of the numbers of subjects at risk}
  \item{\dots}{other arguments to be passed in \code{plot.survfit()}}
}

\value{None}
\author{Hugo Varet}

\examples{
cgd$time=cgd$tstop-cgd$tstart
plot_km(Surv(time,status)~sex,data=cgd,lwd=2,col=c("blue","red"))
}
