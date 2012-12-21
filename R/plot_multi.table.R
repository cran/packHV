plot_multi.table <-
function(data,vars){
  tab=multi.table(data,vars)
  par(mar=c(1, 1, 1, 1) + 0.1)
  plot.new()
  plot.window(xlim=c(0,ncol(tab)+3),ylim=c(0,ncol(tab)+3))
  text(rep(2:(ncol(tab)+1),ncol(tab)),rep(ncol(tab):1,each=ncol(tab)),tab)
  text(1,ncol(tab):1,colnames(tab),srt=90,pos=2)
  text(2:(ncol(tab)+1),ncol(tab)+1,colnames(tab),pos=4)
}
