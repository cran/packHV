plot_reg <-
function(x,y,pch=19,xlab=NULL,ylab=NULL,...){
  # x and y are the two variables (missing values are not supported)
  # ... are arguments to be passed in plot() 
  abs <- deparse(substitute(x))
  ord <- deparse(substitute(y))
  
#  eval(parse(text=paste(abs,"=x",sep="")))
#  eval(parse(text=paste(ord,"=y",sep="")))  
#  eval(parse(text=paste("fit=lm(",ord,"~",abs,")",sep="")))
  
  fit=lm(y~x)
  print(summary(fit))
  if (is.null(xlab)){xlab=abs}
  if (is.null(ylab)){ylab=ord}
  d=data.frame(abs=x,ord=y)
  plot(d$abs,d$ord,pch=pch,xlab=xlab,ylab=ylab,...)
  abline(fit,lwd=2)
}
