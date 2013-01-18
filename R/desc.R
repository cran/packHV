desc <-
function(data,vars,group=NULL,vars.labels=vars,group.labels=NULL,type.quanti="med",test=TRUE,
              noquote=TRUE,justify=TRUE,digits=2,file.export=NULL,language="english"){
  # data: the data frame in which we can find
  #   vars: c("var1",..,"varN")
  #   group: NULL to describe the whole population
  #          or "group" to describe each level of group and to test the impact
  #          of "group" on each variable in vars
  # type.quanti: "mean" returns mean (sd) and "med" returns med [Q1,Q3] for quantitative covariates (mean_mm or med_mm to add (min;max))
  # test: boolean, if TRUE tests will be performed:
  #   chisq/fisher exact test for qualitative covariates (depending on whether any cell number is <5),
  #   wilcoxon/kruskal if type.quanti="med" for quantitative covariates
  #   t/anova if type.quanti="mean" for quantitative covariates
  # noquote: boolean, if TRUE "" are deleted when printing the table (must be FALSE to export in Latex)
  # justify: boolean, if TRUE columns are justified on the left or on the right (FALSE if export)
  # digits: number of digits of the statistics (mean, sd, median, min, max, Q1, Q3, %), p-values have always 3 digits
  # file.export: character string of the path where to export the returning table
  # language: character string "english" or "french" which can change the name of the columns (moreover, french will replace "." by ",")
  export=!is.null(file.export)
  if (export){justify=FALSE}
  name_data=deparse(match.call()[[2]])
  if (!is.data.frame(data)){
    stop(paste(name_data," is not a data frame",sep=""))
  }
  if (!I(language %in% c("english","french"))){
    stop("language must be equal to \"english\" or \"french\"")
  } else{
    if (language=="english"){
      whole_pop="Whole sample"; pvalue="p-value"; parameter="Covariate";
    } else{
      whole_pop="Echantillon entier"; pvalue="p-valeur"; parameter="Variable";
    }
  }
  if (is.null(group)){
    test=FALSE; groupname=""; group=factor(rep(whole_pop,nrow(data))); sep="";
  } else{
    if (!I(group %in% names(data))){
      stop(paste("The variable of interest ",group," is not in ",name_data,sep=""))
    }
    groupname=group
    if (any(is.na(data[,groupname]))){cat(paste(sum(is.na(data[,groupname]))," missing values in ",groupname,"\n",sep=""))}  
    sep=" "; data=data[!is.na(data[,groupname]),]; group=factor(data[,groupname]);
  }
  if (length(vars)!=length(vars.labels)){
    stop("vars and vars.labels do not have the same length")
  }
  levgp=levels(group)
  nb.group=length(levgp)
  vars=unique(vars)
  if (any(!I(vars %in% names(data)))){
    cat(paste("Variable(s) ",paste(vars[!I(vars %in% names(data))],collapse=", ")," not in ",name_data,"\n",sep=""))
    vars.labels=vars.labels[vars %in% names(data)]
    vars=vars[vars %in% names(data)]
  }
  if (!I(type.quanti %in% c("mean","med","mean_mm","med_mm"))){
    stop("type.quanti must be equal to \"mean\", \"med\", \"mean_mm\" or \"med_mm\"")
  } else{
    wt=ifelse(test," and Wilcoxon test","")
    tt=ifelse(test," and t-test","")
    khi2_fisher=ifelse(test," and khi-2 or Fisher exact test","")
    if (type.quanti=="med"){
      fun.quanti=function(var){
        q=round(quantile(var,probs=c(0.25,0.5,0.75),na.rm=TRUE),digits)
        return(paste(q[2]," [",q[1],";",q[3],"]",sep=""))
      }
      cat(paste("med [Q1;Q3]",wt," for numeric variables\n",sep=""))    
    } 
    if (type.quanti=="med_mm"){
      fun.quanti=function(var){
        q=round(quantile(var,probs=c(0,0.25,0.5,0.75,1),na.rm=TRUE),digits)
        return(paste(q[3]," [",q[2],";",q[4],"]"," (",q[1],";",q[5],")",sep=""))
      }
      cat(paste("med [Q1;Q3] (min;max)",wt," for numeric variables\n",sep=""))    
    } 
    if (type.quanti=="mean"){
      fun.quanti=function(var){
        return(paste(round(mean(var,na.rm=TRUE),digits)," (",round(sd(var,na.rm=TRUE),digits),")",sep=""))
      }
      cat(paste("mean (sd)",tt," for numeric variables\n",sep=""))    
    }
    if (type.quanti=="mean_mm"){
      fun.quanti=function(var){
        return(paste(round(mean(var,na.rm=TRUE),digits)," (",round(sd(var,na.rm=TRUE),digits),")",
                     " (",round(min(var,na.rm=TRUE),digits),";",round(max(var,na.rm=TRUE),digits),")",sep=""))
      }
      cat(paste("mean (sd) (min;max)",tt," for numeric variables\n",sep=""))    
    }
  }
  cat(paste("N (%)",khi2_fisher," for categorical variables\n",sep=""))
  ncol=2+nb.group+1*test
  out=matrix("",nrow=2,ncol=ncol)
  if (is.null(group.labels)){
    colnames=paste(groupname,levgp,sep=sep)
  } else{
    if (length(group.labels)!=length(levgp)){stop("the argument 'group.levels' does not fit the argument 'group'")}
    colnames=group.labels
  }
  if (test){
    out[1,]=c(parameter,"",colnames,pvalue)
    out[2,]=c("","",paste("(N=",table(group),")",sep=""),"")       
  } else{
    out[1,]=c(parameter,"",colnames)
    out[2,]=c("","",paste("(N=",table(group),")",sep=""))       
  }    
  types=NULL; for (i in 1:length(vars)){types=c(types,class(data[,vars[i]]))}   # deleting covariates != numeric, character or factor
  delete=vars[!I(types %in% c("integer","numeric","factor","character"))]
  if (length(delete)>0){cat(paste("Variable(s) ",paste(delete,collapse=", ")," not numeric, neither factor, neither character\n",sep=""))}
  vars.labels=vars.labels[!I(vars %in% delete)]
  vars=vars[!I(vars %in% delete)]
  for (i in 1:length(vars)){                                                    # loop for each covariate
    name_var=vars.labels[i]
    var=data[,vars[i]]
    nb.na=sum(is.na(var))
    if (is.numeric(var)){                                                       # for each numeric variable
      res=NULL
      for (lev_i in levgp){
        res=c(res,fun.quanti(var[group==lev_i]))
      }
      p.value=NULL
      if (test){
        if (type.quanti %in% c("mean","mean_mm")){
          if (nb.group>2){
            p.value=anova(lm(var~group))[1,5]
            if (is.na(p.value)){cat(paste("Problem of test with covariate",vars[i],"\n"))}
          } else{
            p.value=NA
            tryCatch(p.value<-t.test(var~group)$p.value,
                     error=function(e){cat(paste("Error: problem of test with covariate",vars[i],"\n"))},
                     warning=function(w){cat(paste("Warning: problem of test with covariate",vars[i],"\n"))})
          }
        }
        if (type.quanti %in% c("med","med_mm")){
          if (nb.group>2){
            p.value=NA
            tryCatch(p.value<-kruskal.test(var~group)$p.value,
                     error=function(e){cat(paste("Error: problem of test with covariate",vars[i],"\n"))},
                     warning=function(w){cat(paste("Warning: problem of test with covariate",vars[i],"\n"))})
          } else{
            p.value=NA
            tryCatch(p.value<-wilcox.test(var~group)$p.value,
                     error=function(e){cat(paste("Error: problem of test with covariate",vars[i],"\n"))},
                     warning=function(w){cat(paste("Warning: problem of test with covariate",vars[i],"\n"))})
          }
        }
      }
      if (!is.null(p.value)){p.value=round(p.value,3)}
#      if (any(is.na(var))){
#        name_var=paste(name_var," (",sum(is.na(var))," NA)",sep="")
#      }
      out=rbind(out,c(name_var,"",res,p.value))
      if (any(is.na(var))){
        out.na=table(is.na(var),group)
#        out.na=paste(out.na[2,]," (",round(100*out.na[2,]/colSums(out.na),2),"%)",sep="")
        out.na=out.na[2,]
        if (!is.null(p.value)){
          out=rbind(out,c("","NA",out.na,""))
        } else{
          out=rbind(out,c("","NA",out.na))
        }
      }
    } else{                                                                     # for each factor variable
      var=factor(var)                                                           # transforming character and factor in factor
      tab=table(var,group)
      tab.p=round(100*prop.table(tab,margin=2),digits) # tab/matrix(apply(tab,2,sum),nrow=nrow(tab),ncol=nb.group,byrow=T)
      if (test){
        if (nrow(tab)<=1){
          p.value=NA
        } else{
          if (any(tab<5)){
            p.value=NA
            tryCatch(p.value<-fisher.test(tab)$p.value,
                     error=function(e){cat(paste("Error: problem of test with covariate",vars[i],"\n"))},
                     warning=function(w){cat(paste("Warning: problem of test with covariate",vars[i],"\n"))})
          } else{
            p.value=NA
            tryCatch(p.value<-chisq.test(tab)$p.value,
                     error=function(e){cat(paste("Error: problem of test with covariate",vars[i],"\n"))},
                     warning=function(w){cat(paste("Warning: problem of test with covariate",vars[i],"\n"))})
          }
        }
      }
      if (all(is.na(var))){
        res.out=matrix("",nrow=1,ncol=ncol)
        res.out[1,1:2]=c(name_var,"NA")
        if (test){
          res.out[1,ncol]=round(p.value,3)
        }  
        res.out[1,3:(2+nb.group)]=table(group)      
      } else{
        res=matrix(paste(tab," (",tab.p,"%)",sep=""),ncol=ncol(tab))
        res.out=matrix("",nrow=nrow(res)+1*I(any(is.na(var))),ncol=ncol)
        res.out[1,1]=name_var
        if (test){
          res.out[1,ncol]=round(p.value,3)
        }
        res.out[1:nrow(res),3:(2+nb.group)]=res
        if (any(is.na(var))){
          res.out[1:nrow(res.out),2]=c(levels(var),"NA")
          res.out[nrow(res.out),3:(2+nb.group)]=table(is.na(var),group)[2,]
        } else{
          res.out[1:nrow(res.out),2]=levels(var)
        }
      }
      out=rbind(out,res.out)
    }
  }
  if (test){
    out[,ncol(out)]=ifelse(out[,ncol(out)]==0,"<0.001",out[,ncol(out)])
    out[,ncol(out)]=ifelse(is.na(out[,ncol(out)]),"---",out[,ncol(out)])
  }
  dimnames(out)=list(rep("",nrow(out)),rep("",ncol(out)))
  if (language=="french"){out[-1,-c(1,2)]=gsub("\\.", ",", out[-1,-c(1,2)])}    # replace . by , (\\. ou [.])  
  if (justify){
    out[,1]=format(out[,1],justify="right")
    out[,2]=format(out[,2],justify="left")
    out[,-c(1:2,ncol(out))]=format(out[,-c(1:2,ncol(out))],justify="right")
    out[,ncol(out)]=format(out[,ncol(out)],justify="right")  
  }  
  if (all(out[,2]==rep("",nrow(out)))){out=out[,-2]}
  if (export){
#    write.xlsx2(data.frame(out),file.export,col.names=F,row.names=F)           # package xlsx
#    writeWorksheetToFile(file.export,data.frame(out),"descHV.export",header=F,rownames=NULL)  # package XLConnect
    out.df=as.data.frame(out)
    WriteXLS("out.df",file.export,"Descriptive statitics",Encoding="latin1",AdjWidth=TRUE,col.names=FALSE)
#    write.table(out,file=file.export,row.names=F,col.names=F,quote=F,sep="\t")
    cat("Export created: ",file.export,"\n",sep="")
    return(invisible(out))
  } else{
    if (noquote){out=noquote(out)}
    return(out)
  }
}
