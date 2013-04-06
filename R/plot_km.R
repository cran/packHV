plot_km <-
function(formula,data,test=TRUE,conf.int=FALSE,times.print=NULL,xlab=NULL,ylab=NULL,
                 left=4.5,bottom=5,cex.mtext=1,lwd=2,col=NULL,...){
  # formula: Surv(temps,cens)~groupe, ces variables étant dans data
  # times.print: temps auxquels les n at risk s'affichent
  # left: nb de lignes dans la marge de gauche (controle donc la largeur)
  # bottom: nb de lignes en plus de celles du tableau dans la marge du bas (utile pour juxtaposer pls graph)
  # attention: dans formula, il ne faut que des noms de variables, i.e. pas de cut(x,...)
  formula <- deparse(substitute(formula))

  # extracting the variable names from formula
  formula <- paste(formula, collapse=" ")
  formula <- gsub("[[:space:]]+", " ", formula)
  varnames <- gsub("\\<Surv\\>|\\(|\\)| \\?\\(.*\\)", "", formula)
  varnames <- strsplit(varnames,  "[[:space:]]*(\\+|,|~)[[:space:]]*")
  varnames <- gsub("[[:space:]]+", "", unlist(varnames))

  if (any(!I(setdiff(varnames,"1") %in% names(data)))){
    stop(paste(paste(varnames,collapse=" and/or ")," not in ",deparse(substitute(data)),"\n",sep=""))
  }
  
  temps=data[,varnames[1]]
  cens=data[,varnames[2]]
  
  if (varnames[3]=="1"){
    groupe=factor(rep("# at risk",nrow(data)))
    name_at_risk=""
    test=F
  } else{
    groupe=factor(data[,varnames[3]])
    if (length(levels(factor(data[,varnames[3]])))==1){
      stop(paste(varnames[3]," has only one level\n",sep=""))
    }
    name_at_risk="# at risk"
  }
    
  d=data.frame(temps,cens,groupe)
  if (any(is.na(d$temps) | is.na(d$cens) | is.na(d$groupe))){
    cat(paste(sum(is.na(d$temps) | is.na(d$cens) | is.na(d$groupe))," rows deleted due to missing values\n",sep=""))
  }
  d=d[I(!is.na(d$temps) & !is.na(d$cens) & !is.na(d$groupe)),]
  
  if (is.null(xlab)){xlab=varnames[1]}
  if (is.null(ylab)){ylab="Survival"}
  if (is.null(col)){col=1:nlevels(d$groupe)}
    
  if (is.null(times.print)){
    times.print=c(0:4)*max(d$temps)/4
  }
  n.risk=matrix(NA,nrow=1+nlevels(groupe),ncol=length(times.print),dimnames=list(c("temps",levels(groupe)),NULL))
  n.risk[1,]=times.print
  for (lev in levels(groupe)){
    tmp=d[d$groupe==lev,]
    tmp2=summary(survfit(Surv(temps,cens)~1,data=tmp),times.print)$n.risk
    if (length(tmp2)<length(times.print)){
      n.risk[lev,]=c(tmp2,rep(0,length(times.print)-length(tmp2)))
    } else{
      n.risk[lev,]=tmp2
    }
  }
  n.risk[2:nrow(n.risk),1]=table(groupe)

  if (test){
    diff=survdiff(Surv(temps,cens)~groupe,data=d)
    p.value=1-pchisq(diff$chisq,df=length(levels(d$groupe))-1)
    p.value=paste("p",ifelse(p.value<0.001,"<0.001",paste("=",round(p.value,3),sep="")),sep="")
  }

#  mar=c(bottom, left, top, right)
  par(mar=c(nrow(n.risk)+bottom, left, 4, 3) + 0.1,xaxs="i",yaxs="i")
  plot(survfit(Surv(temps,cens)~groupe,data=d),conf.int=conf.int,xlab=xlab,ylab=ylab,lwd=lwd,col=col,...)
  if (test){text(max(d$temps)/20,0.05,p.value,adj=c(0,0))}  
  mtext(side=1,at=-0.065*max(times.print),line=4,name_at_risk,cex=cex.mtext,adj=1)
  for (i in 2:nrow(n.risk)){
    mtext(side=1,at=times.print,line=i+3,n.risk[i,],cex=cex.mtext)
    mtext(side=1,at=-0.065*max(times.print),line=i+3,rownames(n.risk)[i],cex=cex.mtext,adj=1)
  }  
}
