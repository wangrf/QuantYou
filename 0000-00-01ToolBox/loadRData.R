loadRData <- function(sym,overlap=T,sumData=F){
  
  if(substr(getwd(),1,1)=="D"){
    dataPath="D:/shares/Work/Project/0000-00-02Data"
  if(sumData==T){
    dataPath="D:/shares/Work/Project/0000-00-04SumData"
  }
    
    }else{
    dataPath="E:/shares/Work/Project/0000-00-02Data"
    if(sumData==T){
      dataPath="E:/shares/Work/Project/0000-00-04SumData"
    } 
  }
  
  
  
  if(overlap==T){
    load(file.path(dataPath,paste0(sym,".RData")),envir=.GlobalEnv)
    print(paste0("original Data for ",sym," is overlapped by this new one"))
    
  }else{
    if(sym %in% ls(envir=.GlobalEnv)){
      print(paste0("Data for ",sym," is exisit in GlobalEnv"))
    }else{
      load(file.path(dataPath,paste0(sym,".RData")),envir=.GlobalEnv)
    }
    
  }
    
  
}
