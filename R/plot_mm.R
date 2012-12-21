plot_mm <-
function(formula,data,col.spag=1,col.mean=1,type="spaghettis",at.x=NULL,at.y=NULL,
                   xlab=NULL,ylab=NULL,main="",lwd.spag=1,lwd.mean=4){
  # formula: obs~time+(group|id) or obs~time+(1|id)
  # data: data frame in which we can find obs, time, group and id
  # col.spag: vector of length nrow(data) with colors (one for each individual)
  # col.mean: vector of length length(levels(group)) with colors (one for each group)
  # type: "spaghettis", "mean" or "both"
  call=match.call()
  formumla=deparse(call[[2]])
  name_data=deparse(call[[3]])
  formula <- paste(formula, collapse=" ")
  formula <- gsub("[[:space:]]+", " ", formula)
  varnames <- gsub("\\||\\+|~[[:space:]]|\\(|\\)| \\?\\(.*\\)", "", formula)
  varnames <- strsplit(varnames,  "[[:space:]]*(\\+|,|~)[[:space:]]*")
  varnames <- gsub("  ", " ", varnames[[1]])      
  varnames <- strsplit(varnames,  "[[:space:]]")[[1]]
  if (!all(varnames[varnames!="1"] %in% names(data))){
    stop(paste("at least one covariate is not in ",name_data,"\n",sep=""))
  }
  
  id=data[,varnames[4]]; time=data[,varnames[2]]; obs=data[,varnames[1]];
  if (varnames[3]==1){group=rep(1,nrow(data))} else{group=data[,varnames[3]]}
  data=data.frame(id,time,obs,group,col.spag=col.spag)
  data$group=factor(data$group)
  data=data[order(data$time),]; data=data[order(data$id),];
  
  if (!I(type %in% c("spaghettis","mean","both"))){
    stop("type must be equal to \"spaghettis\", \"mean\" or \"both\"")
  }

  if (is.null(xlab)){xlab=varnames[2]}
  if (is.null(ylab)){ylab=varnames[1]}  
  if (is.null(at.x)){at.x=unique(data$time)}
  if (is.null(at.y)){at.y=seq(min(data$obs,na.rm=TRUE),max(data$obs,na.rm=TRUE),length=4)}
  
  plot.new()
  plot.window(xlim=range(data$time,na.rm=TRUE),ylim=range(data$obs,na.rm=TRUE))
  axis(1,at=at.x); axis(2,at=at.y);
  title(main=main,xlab=xlab,ylab=ylab)
  if (type %in% c("spaghettis","both")){                                        # spaghettis
    for (i in unique(data$id)){
      data.i=data[data$id==i,]
      lines(data.i$time,data.i$obs,col=data.i$col.spag[1],lwd=lwd.spag)
    }
  }
  if (type %in% c("mean","both")){                                              # means
    for (i in 1:length(levels(data$group))){
      data.g=data[data$group==levels(data$group)[i],c("time","obs")]
      data.g=aggregate(data.g,by=list(data.g$time),FUN=function(x){mean(x,na.rm=TRUE)})
      col.g=col.mean[i]
      lines(data.g$time,data.g$obs,col=col.g,lwd=lwd.mean)  
    }
  }
}
