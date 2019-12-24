
lm.value<-function(x){
  
  yy<-as.numeric(x)
  
  yy<-yy[!is.na(yy)]
  yy<-yy/yy[1]
  
  if(length(yy)<5) {return(0)}else{
    xx<-1:length(yy)
    
    res.lm<-summary(lm(yy~xx))
    return( res.lm$coefficients[2] * res.lm$r.squared)
    
  }
  
}


Return.calculateX<-function(x,column){
  
  id<-grep(column,names(x))
  prices<-x[,id]
  Return.calculate(prices)
}

Return.cumulative.max <- function(x){
  
  max(rollapply(x,length(x),Return.cumulative))
  
}



rollapplyX<-function(x,column,width,FUN,if.column=F){
  
  id<-match.names(column,names(x))
  
  data<-x[,id]
  myfun=match.fun(FUN)
  rollapply(data,width,myfun,by.column=if.column)
  
}




apply.fromstartX<-function(x,column,FUN){
  
  id<-match.names(column,names(x))
  data<-x[,id]
  myfun=match.fun(FUN)
  xts(rollapply(zoo(data),nrow(data),myfun,partial = T),order.by=index(x))
  
}


leadX<-function(x,column,k){
  
  id<-match.names(column,names(x))
  data<-as.vector(x[,id])
  data<-lead(data,k)
  xts(data,order.by=index(x))
  
}

lagX<-function(x,column,k){
  
  id<-grep(column,names(x))
  data<-x[,id]
  data<-lag.xts(data,k)
  data
  
}



multipleX<-function(x,column,f){
  
  id<-grep(column,names(x))
  data<-x[,id]
  
  data*f
  
}

minusX<-function(x,column1,column2){
  
  id1<-match.names(column1,colnames(x))
  id2<-match.names(column2,colnames(x))
  
  x[,id1]-x[,id2]
  
}

SMAX <- function(x, column, nDay) {
  idx <- match.names(column, colnames(x))
  y <- SMA(x[, idx], n = nDay)
  names(y) <- "SMA"
  y
}

EMAX <- function(x, column, nDay) {
  idx <- match.names(column, colnames(x))
  
  y <- EMA(x[, idx], n = nDay,wilder = T)
  names(y) <- "SMA"
  y
}

peakValleyX<-function(x,column,thresh){
  idx <- match.names(column, colnames(x))
  ss<-x[,idx]
  ss[is.na(ss)]<-0
  y<-rep(0,nrow(x))
  p1<-findPeaks(ss,thresh)
  v1<-findValleys(ss,thresh)
  y[p1]<-1
  y[v1]<--1
  y<-xts(y,order.by=index(x))
  names(y)<-"peakValley"
  y
  
}

peakValleyNewX<-function(x,column,thresh){
  
  
  nDay<-thresh
  ind<-match.names(column,colnames(x))
  ind.name<-colnames(x)[ind]
  x<-x[,ind]
  
  x.max<-rollapply(x,nDay,max)
  x.min<-rollapply(x,nDay,min)
  
  x.max.lead<-xts(lead(as.numeric(x.max),nDay),order.by=index(x.max))
  x.min.lead<-xts(lead(as.numeric(x.min),nDay),order.by=index(x.min))
  y<-merge(x,x.max,x.min,x.max.lead,x.min.lead)
  
  names(y)<-paste0(ind.name,"_",c("close","past_max","past_min","forward_max","forward_min"))
  
  y.peak<-y[,paste0(ind.name,"_","close")]>=y[,paste0(ind.name,"_","past_max")]&y[,paste0(ind.name,"_","close")]>=y[,paste0(ind.name,"_","forward_max")]
  y.valley<-y[,paste0(ind.name,"_","close")]<=y[,paste0(ind.name,"_","past_min")]&y[,paste0(ind.name,"_","close")]<=y[,paste0(ind.name,"_","forward_min")]
  
  yy<-rep(0,nrow(x))
  yy[which(y.peak)]<- 1
  yy[which(y.valley)]<- -1
  
  yy<-xts(yy,order.by=index(x))
  names(yy)<-"peakVelly.sign"
  
  
  yy
  
}


find_peaks <- function (x, m = 3){
  shape <- diff(sign(diff(x, na.pad = FALSE)))
  pks <- sapply(which(shape < 0), FUN = function(i){
    z <- i - m + 1
    z <- ifelse(z > 0, z, 1)
    w <- i + m + 1
    w <- ifelse(w < length(x), w, length(x))
    if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
  })
  pks <- unlist(pks)
  pks
}

peakValleyY<-function(x,column,thresh){
  idx <- match.names(column, colnames(x))
  ss<-x[,idx]
  ss[is.na(ss)]<-0
  
  ss<-as.vector(ss)
  
  y<-rep(0,nrow(x))
  p1<-find_peaks(ss,m=thresh)
  v1<-find_peaks(-ss,m=thresh)
  y[p1]<-1
  y[v1]<--1
  y<-xts(y,order.by=index(x))
  names(y)<-"peakValley"
  y
  
}

PVcount<-function(x){
  
  nn<-sum(!is.na(x))
  x[is.na(x)]<-0
  sum((x!=0))/nn
  
}





addX<-function(x,column1,column2){
  
  id1<-match.names(column1,colnames(x))
  id2<-match.names(column2,colnames(x))
  
  x[,id1]+x[,id2]
  
}

divX<-function(x,column1,column2){
  
  id1<-match.names(column1,colnames(x))
  id2<-match.names(column2,colnames(x))
  
  x[,id1]/x[,id2]
  
}

na.skip <- function (x, FUN=NULL, ...){ # @author Brian Peterson
  
  # DESCRIPTION:
  
  # Time series data often contains NA's, either due to missing days, 
  # noncontiguous series, or merging multiple series,
  # 
  # Some Calulcations, such as return calculations, require data that 
  # looks like a vector, and needs the output of na.omit
  # 
  # It is often convenient to apply these vector-like functions, but 
  # you still need to keep track of the structure of the oridginal data.
  
  # Inputs
  # x		the time series to apply FUN too
  # FUN	function to apply
  # ...	any additonal parameters to FUN
  
  # Outputs:
  # An xts time series that has the same index and NA's as the data 
  # passed in, after applying FUN
  
  nx <- na.omit(x)
  fx <- FUN(nx, ... = ...)
  if (is.vector(fx)) {
    result <- .xts(fx, .index(x), .indexCLASS = indexClass(x))
  }
  else {
    result <- merge(fx, .xts(, .index(x)))
  }
  return(result)
}

Runups<-function (R, geometric = TRUE, ...) 
{
  x = checkData(R)
  columns = ncol(x)
  columnnames = colnames(x)
  colDrawdown <- function(x, geometric) {
    if (geometric) 
      Return.cumulative = cumprod(1 + x)
    else Return.cumulative = 1 + cumsum(x)
    maxCumulativeReturn = cummin(c(1, Return.cumulative))[-1]
    column.drawdown = Return.cumulative/maxCumulativeReturn - 
      1
  }
  for (column in 1:columns) {
    column.drawdown <- na.skip(x[, column], FUN = colDrawdown, 
                               geometric = geometric)
    if (column == 1) 
      drawdown = column.drawdown
    else drawdown = merge(drawdown, column.drawdown)
  }
  colnames(drawdown) = columnnames
  drawdown = reclass(drawdown, x)
  return(drawdown)
}
maxRunup<-function (R, weights = NULL, geometric = TRUE, 
                    ...) 
{
  if (is.vector(R) || ncol(R) == 1) {
    R = na.omit(R)
    drawdown = Runups(R, geometric = geometric)
    result = max(drawdown)
    return(result)
  }
  else {
    if (is.null(weights)) {
      R = checkData(R, method = "matrix")
      result = apply(R, 2, maxRunup, geometric = geometric, 
                     ... = ...)
      dim(result) = c(1, NCOL(R))
      colnames(result) = colnames(R)
      rownames(result) = "Finnest Runup"
      return(result)
    }
    else {
      portret <- Return.portfolio(R, weights = weights, 
                                  geometric = geometric)
      result <- maxRunup(portret, geometric = geometric, 
                         ... = ...)
      
      return(result)
    }
  }
}



