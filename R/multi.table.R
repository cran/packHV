multi.table <-
function(data,vars){
  call=match.call()
  if (any(!I(vars %in% names(data)))){
    stop(paste(paste(vars[!I(vars %in% names(data))],collapse=", ")," not in ",deparse(call[[2]]),"\n",sep=""))
  }

  names=NULL
  levels=NULL
  vars.pb=NULL
  for (var in vars){
    if (!(is.factor(data[,var]) | is.character(data[,var]))){
      vars.pb=c(vars.pb,var)
    }
  }
  if (length(vars.pb)>0){
    stop(paste(paste(vars.pb,collapse=", ")," not factor neither character",sep=""))
  }
  for (var in vars){names=c(names,var,rep("",nlevels(data[,var])))}
  for (var in vars){levels=c(levels,"",levels(data[,var]))}
  names=names[-length(names)]
  res=matrix("",nrow=length(names),ncol=length(names),dimnames=list(names,names))
  for (var1 in vars){
    for (var2 in vars){
      deb1=which(rownames(res)==var1)
      deb2=which(colnames(res)==var2)
      res[deb1:(deb1+nlevels(data[,var1])-1),deb2:(deb2+nlevels(data[,var2])-1)]=table(data[,var1],data[,var2])
    }
  }
  res2=matrix("",nrow(res)+1,ncol(res)+1)
  colnames(res2)=rownames(res2)=c("",colnames(res))
  res2[-1,-1]=res
  res2[1,]=res2[,1]=levels
  
  res3=matrix("",nrow(res2)+1,ncol(res2)+1)
  colnames(res3)=rownames(res3)=c("",colnames(res2))
  res3[-c(1:2),-c(1:2)]=res2[-1,-1]
  res3[1,]=res3[,1]=c("",res2[1,])
  
  return(noquote(res3))
}
