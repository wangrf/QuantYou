source(file.path(substr(getwd(),1,22),"header.R"))

library(WindR)



indicators = c("pe_ttm")
syms1<-xx[[1]]
startDate='2000-1-1'
endDate='2019-12-12'

#sapply(syms1,updateStock,startDate=startDate,endDate=endDate,indicators=indicators,dataPath=dataPath)
