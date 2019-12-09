
updateFoir <- function(sym, startDate, endDate, dataPath=dataPath) {
  
  data.list <- dir(pattern = ".RData",path = dataPath)
  idMatch <- grep(paste0(sym,".oir"), data.list)
  nMatch <- length(idMatch)
  
  sym2<-paste0(sym,".oir")
  
  
  if (nMatch==0) {
    warning(paste0(sym2, " No history data and it is the first time"))
    
  
    y<-w.wset('futureoir',paste0('startdate=',startDate,';enddate=',endDate,';varity=',sym,';order_by=long;ranks=all'))$Data
    y$date<-as.Date(y$date,origin="1899-12-30")
    y<-y[!is.na(y$ranks),]
    
    assign(sym2,y)
    save(list=c(sym2),file=paste0(dataPath,"/",sym2,".RData"))
    print(paste0("Have updated ",paste0(sym,".oir",collapse = ","),"",",and update the date to ",endDate))
    
    
  } else if (nMatch > 1) {
    stop(paste0("Exits multiple RData", "for ",sym2))
  } else{
    
    load(paste0(dataPath, "/", data.list[idMatch]),envir = .GlobalEnv)
    
    x <- NULL
    x <- get(sym2)
    last.date <- max(as.Date(x$date, "%Y-%m-%d"))
    if(last.date<as.Date(endDate,"%Y-%m-%d")){
   
      startDate <- last.date+1
      
      ### update the date
      
      y<-w.wset('futureoir',paste0('startdate=',startDate,';enddate=',endDate,';varity=',sym,';order_by=long;ranks=all'))$Data
      y$date<-as.Date(y$date,origin="1899-12-30")
      y<-y[!is.na(y$ranks),]
      
      x <- rbind(x, y)
      
      
      assign(sym2, x)
      save(list=c(sym2),file=paste0(dataPath,"/",sym2,".RData"))
      print(paste0("Have updated ",paste0(sym2,collapse = ",")," data",",and update the date to ",endDate))
      
    }else{
      print(paste0("There is no need to update,the ",sym2," is the newest"))
    }
  }
  
}
