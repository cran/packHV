#' Plot a histogram with a boxplot below
#'
#' Plots a histogram with a boxplot below
#'
#' @param x a numeric vector
#' @param freq boolean, \code{TRUE} for frequency or \code{FALSE} probability on the y axis
#' @param density boolean, \code{TRUE} to plot the estimated density
#' @param main character string, main title of the histogram
#' @param xlab character string, label of the x axis
#' @param ymax numeric value, maximum of the y axis
#' @param \dots other arguments to be passed in \code{hist()}
#' @return None
#' @author Hugo Varet
#' @examples
#' par(mfrow=c(1,2))
#' hist_boxplot(rnorm(100),col="lightblue",freq=TRUE)
#' hist_boxplot(rnorm(100),col="lightblue",freq=FALSE,density=TRUE)

hist_boxplot=function(x,freq=TRUE,density=FALSE,main=NULL,
                      xlab=NULL,ymax=NULL,...){
  abs <- deparse(substitute(x))
  if (is.null(xlab)){xlab=abs}
  ted <- hist(x,plot=FALSE)
  par(yaxs="i")
  if (freq){
    ylim=c(-(max(ted$counts)/5),max(ted$counts))
    boxwex=max(ted$counts)/6
  } else{
    if (density){
      max=max(max(density(x)$y),max(ted$density))
    } else{
      max=max(ted$density)
    }
    ylim=c(-max/5,max)
    boxwex=max/6    
  }
  if (!is.null(ymax)) ylim <- c(-ymax/5,ymax)
  if (is.null(main)){main=paste("Histogram of",abs)}
  hist(x,ylim=ylim,yaxt="n",xlab=xlab,freq=freq,main=main,...)
  axis(2,at=seq(0,ylim[2],length=5))
  usrs <- par()$usr
  par(new=TRUE,bty="n",xaxs="i",yaxs="i",ann=FALSE)
  boxplot(x,horizontal=TRUE,ylim=usrs[1:2],add=TRUE,
          at=usrs[3]/2,pars=list(boxwex=5),col="lightblue",boxwex=ylim[2]/6)
  if (density){
    lines(density(x),lwd=2,col="red")
  }
}
