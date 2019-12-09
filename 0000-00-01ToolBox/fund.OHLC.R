fund.OHLC <- function(sym){
  
  xx<-get(sym,envir=.GlobalEnv)
  xx<-xts(data.frame(open=xx[,1],high=xx[,1],low=xx[,1],close=xx[,1]),order.by=index(xx))
  names(xx) <- paste0(sym,".",c("open","high","low","close"))
  assign(sym,xx,envir = .GlobalEnv)
  
  
}