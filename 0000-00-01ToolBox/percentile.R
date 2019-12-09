percentile<-function(x){
  
  if(sum(is.na(x))==length(x)){
    return(rep(NA,length(x)))
  }
  
  x<-as.numeric(x)
  Fn<-ecdf(x)
  Fn(x)
  
}

last.percentile<-function(x){
  
  tail(percentile(x),1)
  
}
