assignX<-function(var.name,x=get(var.name)){
  
  assign(var.name,x,envir=.GlobalEnv)
  
}