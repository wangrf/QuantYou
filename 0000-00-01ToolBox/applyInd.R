
applyInd<- function(sym,fun.name,arguments,label){
  
  mktdata <- get(sym,envir=.GlobalEnv)
  
  if (any(diff(.index(mktdata)) == 0)) {
    warning("'mktdata' index contains duplicates; calling 'make.index.unique'")
    mktdata <- make.index.unique(mktdata)
  }
  
  ret <- NULL
  indFun <- get(fun.name, mode = "function")
  
  
  .formals <- formals(indFun)
  .formals <- modify.args(.formals, arguments, 
                          dots = TRUE)

  
  tmp_val <- do.call(indFun, .formals)
   
    colnames(tmp_val) <- paste(sym,label,sep=".")
  
  if (nrow(mktdata) == nrow(tmp_val) | length(mktdata) == 
      length(tmp_val)) {
    mktdata <- cbind(mktdata, tmp_val)
  }
 
    print(paste0("create indicator: ", colnames(tmp_val)))
assign(sym,mktdata,envir=.GlobalEnv)
return(sym)

}