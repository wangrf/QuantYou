source(file.path(substr(getwd(),1,22),"header.R"))

syms1<-substr(dir(dataPath,"CI"),1,11)


for(sym in syms1){
  loadRData(sym)
}


close.syms<-merge_ind(syms1,"close")
close.syms<-close.syms["2019-01-04/"]
R.syms<-Return.calculate(close.syms)

table.Monthly(R.syms)

##calculate the monthly return of syms

ss<-sapply(R.syms,table.Weekly,simplify = F)
sx<-sapply(ss,function(x) x[,-54])
sx<-as.data.frame((sx))

