source(file.path(substr(getwd(),1,22),"header.R"))

### calculate the maxDrawndown and max2nowR min2nowR of popular stocks
xx<-read_excel("./Xu-PopularStock/Stocks.xls")

indicators = c("pe_ttm")
syms1<-xx[[1]]
startDate='2000-1-1'
endDate='2019-12-12'

#sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)

for(sym in syms1){
  loadRData(sym)
}


close.syms<-merge_ind(syms1,"close")
close.syms<-close.syms["2019-11-20/"]
R.syms<-Return.calculate(close.syms)

maxD.syms<-maxDrawdown(R.syms)

maxPrice.syms<-apply(close.syms,2,max)
minPrice.syms<-apply(close.syms,2,min)

nowPrice.syms<-tail(close.syms,1)
max2nowR.syms<-nowPrice.syms/maxPrice.syms-1
min2nowR.syms<-nowPrice.syms/minPrice.syms-1

out<-rbind(as.data.frame(-maxD.syms),
           as.data.frame(max2nowR.syms),
           as.data.frame(min2nowR.syms))
out<-cbind(substr(colnames(out),2,10),xx[[2]],t(out))
colnames(out)<-c("代码","名称","最大回撤","最高点至今","最低点至今")
write.csv(out,file="out.csv",row.names=F)
