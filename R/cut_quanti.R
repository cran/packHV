cut_quanti <-
function(x,n,...){
  out=cut(x,breaks=quantile(x,probs=seq(0,1,length=n+1)),include.lowest=TRUE,...)
  return(out)
}
