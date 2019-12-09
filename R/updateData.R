## ???ݸ??¹???
### һֻ֤ȯһ??RData
### Row-ָ??,Column-????
### xts()??ʽ
### ָ??ǰ׺Ϊ֤ȯ????,eg: 000300.SH.Close
### ָ????????ĸ??д,eg: Close,Open

library(WindR)
library(xts)
library(purrr)
library(dplyr)
library(readxl)
w.start()

mypath <- getwd()
upPath <- paste0(
  substr(mypath, 1, max(unlist(
    gregexpr("/", mypath, useBytes = T)
  ))))
toolPath <- paste0(upPath,"0000-00-01ToolBox")
dataPath <- paste0(upPath,"0000-00-02Data")
sumPath<-paste0(upPath,"0000-00-04SumData")

sapply(paste0(toolPath,"/",dir(toolPath,pattern = ".R")),source)


startDate='2000-1-1'
endDate='2019-10-25'

updateFoir("IF.CFE","2013-10-9","2014-10-8",dataPath)
loadRData("IF.CFE.oir")
x<-rbind(x,IF.CFE.oir)



indicators = c("close","oi","oi_chg")
syms1<-c("IF00.CFE","IF01.CFE","IF02.CFE","IF03.CFE",
         "IH00.CFE","IH01.CFE","IH02.CFE","IH03.CFE",
         "IC00.CFE","IC01.CFE","IC02.CFE","IC03.CFE",
         "T.CFE")


sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)


###------------------update momData-------------------------
startDate='2000-1-1'
endDate='2019-7-5'

indicators = c("pb_lyr")
syms1<-substr(dir(dataPath,"CI"),1,11)
sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)

# indicators = c("close","pe_ttm")
# syms1<-w.wset('sectorconstituent','date=2019-01-03;windcode=000300.SH')$Data$wind_code
# sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)
# 
# indicators = c("close","pe_ttm")
# syms1<-w.wset('sectorconstituent','date=2019-01-03;windcode=000016.SH')$Data$wind_code
# sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)



indicators = c("pe_ttm","dividendyield2","pb_lf")
syms1<-c("000905.SH","000300.SH","000001.SH","000016.SH","399001.SZ","399005.SZ","399006.SZ")
sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)

indicators = c("nav_adj")
syms1<-c("003003.OF")
sapply(syms1,updateFund,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)


loadRData("sym50")
indicators = c("pe_ttm","dividendyield2")
sapply(sym50,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)


indicators = c("pe_ttm","dividendyield2")
sym="HSI.HI"
sapply(sym,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)


###-----------update sec name--------------------------
code<-dir(dataPath,pattern = "(.SZ)|(.SH)|(.OF)|(.HI)")
for(i in 1:length(code)){
  code[i] <- substr(code[i],1,nchar(code[i])-6)
}
updateName(code,dataPath=dataPath)

###----------------------------
edb.day<-read_excel("日频EDB.xls",skip=1)
edb.day<-edb.day[-1,]
edb.day[,1]<-as.Date(as.numeric(edb.day[[1]]),origin="1899-12-30")
edb.day[,2]<-as.numeric(edb.day[[2]])
names(edb.day)<-c("Date","BondYield.10Y")
edb.day<-xts(edb.day[,-1],order.by=edb.day[[1]])

save(edb.day,file="edb.day.RData")

###-------------------------------------

indicators = c("nav_adj")
XX<-read_excel("C:\\Users\\Administrator\\Desktop\\EE.xlsx")

syms1<-XX[[1]]


sapply(syms1,updateFund,startDate=startDate,endDate=endDate,indicators=indicators)


startDate='2000-1-1'
endDate='2018-11-16'

indicators = c("pe_ttm","turn","yoynetprofit")
syms1<-c("000001.SH")
sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)

indicators=c("pe_ttm","profit_ttm")
syms1<-c("600519.SH")
sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)




indicators = c("close")
syms1<-c("830009.XI","CN.SG")
sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)

indicators = c("pe_ttm")
syms1<-"600519.SH"
sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)



syms<-c(paste0("88200",1:9,".WI"),paste0("8820",10:11,".WI"))
indicators=c("close")
sapply(syms,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)




xx<-read_excel("aa.xlsx")#IF月合�?
indicators="close"
mapply(updateStock,sym=unlist(xx[,1]),startDate=as.Date(unlist(xx[,2]),"%Y-%m-%d"),endDate=as.Date(unlist(xx[,3]),"%Y-%m-%d"),indicators="close",dataPath=dataPath)


A.all<-w.wset('sectorconstituent','date=2018-11-09;windcode=881001.WI')$Data


indicators=c("close")
syms1<-A.all[21:50,3]
sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)

syms<-c("000001.SH","881001.WI","881003.WI")
indicators=c("close","yoynetprofit","pe_ttm","netprofit_ttm")
sapply(syms,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)




### -------------EDB数据更新 --------------

edb.day <- read_excel("日频EDB.xls",col_types = c("guess",rep("numeric",20)))


### ———————————涨停股�?---

syms.allA<-w.wset('sectorconstituent',date=endDate,'sectorid=a001010100000000')$Data
maxUpDown<-w.wsd(syms.allA$wind_code,"maxupordown",endDate,endDate)$Data
syms.maxUpDown<-maxUpDown[maxUpDown[,2]==1,"CODE"]
syms.maxUpDown<-syms.maxUpDown[!is.na(syms.maxUpDown)]
save(syms.maxUpDown,file=file.path(sumPath,"syms.maxUpDown.RData"))

indicators=c("close","pct_chg","maxupordown")
sapply(syms.maxUpDown,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)



nn<-dir(pattern = ".RData")[3:586]
for(n1 in nn){
  n2<-substr(n1,1,nchar(n1)-6)
  loadRData(n2)
  xx<-get(n2)
  
  idx<-max(max(which(is.na(xx[,1]))),ifelse(is.infinite(max(which(get(n2)[,1]==0))),0,max(which(get(n2)[,1]==0))))
  
  rr<-nrow(xx)
  
  if(idx==rr){
    file.remove(paste0(n2,".RData"))
  }else{
    xx<-xx[(idx+1):(rr),]
    assign(n2,xx,envir=.GlobalEnv)
    
    save(list=c(n2),file=paste0(n2,".RData"))
    
  }
  
}


ifelse(is.infinite(max(which(get(symA)[,4]==0))),0,max(which(get(symA)[,4]==0)))