
updateName <- function( sym,dataPath=dataPath) {
  
  loadRData("secName")
  
  endDate<-w.tdaysoffset(0,"2019-03-10")$Data[1,1]
  
    y <-
      w.wsd(sym,
            "sec_name",
            endDate,
            endDate
            )$Data
       
    names(y)<-c("code","sec_Name")
  idx<-which(is.na(match(y[,"code"],secName$code)))
  y<-y[idx,]
  secName<-rbind(secName,y)
    save(secName,file=file.path(dataPath,"secName.RData"))
    print(paste0("Have updated the following codes: "))
    print(y)
    
  
}
