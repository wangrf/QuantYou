
get.PV<-function(startDate,endDate,dataPath){
  
  indicators = c("close")
  syms1<-c("000905.SH","000300.SH","000001.SH","000016.SH","399001.SZ","399005.SZ","399006.SZ")
  sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)
  
  
}

get.OI<-function(startDate,endDate,dataPath){

    indicators = c("close","oi","oi_chg")
  syms1<-c("IF00.CFE","IF01.CFE","IF02.CFE","IF03.CFE",
           "IH00.CFE","IH01.CFE","IH02.CFE","IH03.CFE",
           "IC00.CFE","IC01.CFE","IC02.CFE","IC03.CFE")
  
  
  sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)
  
  
}

get.catchUp<-function(startDate,endDate,dataPath){
  
  indicators = c("close")
  syms1<-substr(dir(dataPath,"^(8820)"),1,9)
  sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)
  
  indicators = c("close")
  syms1<-substr(dir(dataPath,"CI"),1,11)
  sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)
  
}

get.mom<-function(startDate,endDate,dataPath){
  
  indicators = c("close","pe_ttm","dividendyield2","turn")
  syms1<-paste0("CI","00",5001:5029,".WI")
  sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)
  
  indicators = c("pe_ttm","dividendyield2")
  syms1<-c("000905.SH","000300.SH","000001.SH","000016.SH","399001.SZ","399005.SZ","399006.SZ")
  sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)
  
  indicators = c("nav_adj")
  syms1<-c("003003.OF")
  sapply(syms1,updateFund,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)
  
}


get.maxUpDown<-function(startDate,endDate,dataPath){
  
  syms.allA<-w.wset('sectorconstituent',date=endDate,'sectorid=a001010100000000')$Data
  maxUpDown<-w.wsd(syms.allA$wind_code,"maxupordown",endDate,endDate)$Data
  syms.maxUpDown<-maxUpDown[maxUpDown[,2]==1,"CODE"]
  syms.maxUpDown<-syms.maxUpDown[!is.na(syms.maxUpDown)]
  save(syms.maxUpDown,file=file.path(sumPath,"syms.maxUpDown.RData"))
  
  indicators=c("close","pct_chg","maxupordown")
  
  if(length(syms.maxUpDown)>0){
    sapply(syms.maxUpDown,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)
    
  }
  
  
  
}

get.secName<-function(dataPath){
  
  code<-dir(dataPath,pattern = "(.SZ)|(.SH)|(.OF)|(.WI)")
  for(i in 1:length(code)){
    code[i] <- substr(code[i],1,nchar(code[i])-6)
  }
  updateName(code,dataPath=dataPath)
}

get.bond<-function(startDate,endDate,dataPath){
  
  indicators = c("close","chg")
  syms1<-"T.CFE"
  sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)
  
  ## 国债收益率 S0059749
  updateEDB("edb.day",startDate,endDate,c("S0059749"),dataPath)
  
}


get.over<-function(startDate,endDate,dataPath){
  
  indicators=c("close")
  
  syms1<-c("600519.SH","000016.SH")
  sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)
  
  
  
  
}



get.weekR<-function(startDate,endDate,dataPath){
  
  indicators=c("close")
  
  
  syms.IDX<-c("000001.SH","399001.SZ","399005.SZ","399006.SZ","000300.SH","000016.SH")
  syms.CI<-substr(dir(dataPath,"CI"),1,11)
  syms<-unique(c(syms.IDX,syms.CI))
  
  sapply(syms,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)
  
}

get.RV<-function(startDate,endDate,dataPath){
  
  indicators=c("close")
  
  
  syms.IDX<-c("000016.SH","399006.SZ","000300.SH","000016.SH")
  
  sapply(syms.IDX,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)
  
}
