
has.ind<-function (x, ind,which = FALSE) 
{
  
  loc <- grep(ind, colnames(x), ignore.case = TRUE)
  if (!identical(loc, integer(0))) {
    return(if (which) loc else TRUE)
  }
  else FALSE
}

indFun<-function (x,ind) {
  if (has.ind(x,ind=ind)) 
    return(x[, grep(ind, colnames(x), ignore.case = TRUE)])
  stop(paste0("subscript out of bounds: no column name containing ",ind))
}

merge_ind<-function(symbols,ind){
  list.sym<-list()
  for(i in 1:length(symbols)){
    list.sym[[symbols[i]]] <- get(symbols[i],envir=.GlobalEnv)
    
  }
  
  do.call(merge,lapply(list.sym,indFun,ind=ind))
}

applyRank <- function(x,rankFun,...){
  FUN <-match.fun(rankFun)
  FUN(x,...)
}

rowRank <- function(x,descreasing=T){
  as.xts(t(apply(ifelse(descreasing,-1,1)*x,1,rank,na.last="keep",ties.method="random")))
  #indexFormat(rr)<-"%Y-%m-%d"
  #return(rr)
}



