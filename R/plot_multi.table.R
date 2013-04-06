plot_multi.table <-
function(data,vars,main=""){
  tab=multi.table(data,vars)
  par(mar=c(1, 1, 1, 1) + 0.1)
  plot.new()
  plot.window(xlim=c(0,ncol(tab)+3),ylim=c(0,nrow(tab)+3))
  title(main=main)
  text(rep(c(1:ncol(tab))+1,nrow(tab)),rep(nrow(tab):1,each=ncol(tab)),t(tab))
  text(1,nrow(tab):1,rownames(tab),srt=90,pos=2)
  text(2:(ncol(tab)+1),nrow(tab)+1,colnames(tab),pos=4)
  par(mar=c(5, 4, 4, 2) + 0.1)
}
