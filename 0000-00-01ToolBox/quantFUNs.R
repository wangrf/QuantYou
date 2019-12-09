
osTotalEq <- function(timestamp,orderqty, portfolio, symbol, ruletype, ...)
{
  ClosePrice <- as.numeric(Op(mktdata[timestamp,]))
  tempPortfolio <- getPortfolio(portfolio.st)
  dummy <- updatePortf(Portfolio=portfolio.st, Dates=paste('::',as.Date(timestamp),sep=''))           
  ### --> when the rule is fired in case of SHY (first symbol in the portfolio), trading.pl will only contain P&L from trading SHY but from SPY     
  trading.pl <- sum(getPortfolio(portfolio.st)$summary$Net.Trading.PL) 
  #  assign(paste("portfolio.",portfolio.st,sep=""),tempPortfolio,pos=.blotter)
  total.equity <- initEq + trading.pl
  orderqty <- total.equity/ClosePrice
  return(orderqty)
}

osInvestAll <- function (data, timestamp, orderqty, ordertype, 
                         orderside, equity, portfolio, symbol, ruletype, ..., initEq) {
  datePos <- format(timestamp,"%Y-%m-%d %H:%M:%OS")
  
  datePos <- strptime(c(datePos), format = "%Y-%m-%d %H:%M:%OS", tz = 
                        "UTC") + 86400 #for daily data
  
  updatePortf(Portfolio=portfolio,Symbol=symbol,Dates=paste0(start(data), 
                                                             "/", datePos))
  # After updating portfolio profit, we can extract the Net.Trading.PL  earned up to datePos.
  trading_pl <- sum(.getPortfolio(portfolio)$summary$Net.Trading.PL)
  # The total equity in the strategy for this symbol (and this symbol only in isolation always, as this is how quantstrat by default works  with applyStrategy)
  equity <- initEq + trading_pl
  ClosePrice <- getPrice(data, prefer = "Close")[datePos]
  UnitSize <- as.numeric((equity / ClosePrice))
  UnitSize1 <- round(UnitSize, digits = 8)
  UnitSize1
}

#'osDollarATR
#'@description computes an order size by way of ATR quantities, as a proportion of tradeSize
#'@param orderside long or short
#'@param tradeSize a notional dollar amount for the trade
#'@param pctATR a percentage of the tradeSize to order in units of ATR. That is, if tradeSize is
#'10000 and pctATR is .02, then the amount ordered will be 200 ATRs of the security.
#'If the last observed ATR is 2, then 100 units of the security will be ordered.
#'@param maxPctATR an upper limit to how many ATRs can be held in a position; a risk limit
#'@param integerQty an integer quantity of shares
#'@param atrMod a string modifier in case of multiples of this indicator being used.
#'Will append to the term 'atr', that is, atrMod of "X" will search for a term called 'atrX'
#'in the column names of the mktdata xts object.
#'@param rebal if TRUE, and current position exceeds ATR boundaries, will automatically sell
#'@export
"osDollarATR" <- function(orderside, tradeSize, pctATR, maxPctATR=pctATR, data, timestamp, symbol,
                          prefer="Open", portfolio, integerQty=TRUE, atrMod="", rebal=FALSE, ...) {
  if(tradeSize > 0 & orderside == "short"){
    tradeSize <- tradeSize*-1
  }
  pos <- getPosQty(portfolio, symbol, timestamp)
  atrString <- paste0("atr",atrMod)
  atrCol <- grep(atrString, colnames(mktdata))
  if(length(atrCol)==0) {
    stop(paste("Term", atrString, "not found in mktdata column names."))
  }
  atrTimeStamp <- mktdata[timestamp, atrCol]
  if(is.na(atrTimeStamp) | atrTimeStamp==0) {
    stop(paste("ATR corresponding to",atrString,"is invalid at this point in time. 
               Add a logical operator to account for this."))
  }
  dollarATR <- pos*atrTimeStamp
  desiredDollarATR <- pctATR*tradeSize
  remainingRiskCapacity <- tradeSize*maxPctATR-dollarATR
  
  if(orderside == "long"){
    qty <- min(tradeSize*pctATR/atrTimeStamp, remainingRiskCapacity/atrTimeStamp)
  } else {
    qty <- max(tradeSize*pctATR/atrTimeStamp, remainingRiskCapacity/atrTimeStamp)
  }
  
  if(integerQty) {
    qty <- trunc(qty)
  }
  if(!rebal) {
    if(orderside == "long" & qty < 0) {
      qty <- 0
    }
    if(orderside == "short" & qty > 0) {
      qty <- 0
    }
  }
  if(rebal) {
    if(pos == 0) {
      qty <- 0
    }
  }
  return(qty)
  }

osDivPos <- function (data, timestamp, orderqty, ordertype, orderside, 
                      portfolio, symbol, ruletype, ..., bondsym,rankName) {
  
  # updatePortf(Portfolio=portfolio.st,Dates=timestamp)
  # updateAcct(account.st,Dates=timestamp)
  # updateEndEq(account.st)
  # 
  # End.Eq<-tail(getAccount(account.st)$summary$End.Eq,1) 
  #
  
  
  if(timestamp == index(data)[1]){
    orderqty = 1e-5
  }else{
    
    End.Eq<-initEq
    
    pos <- getPosQty(portfolio, symbol, timestamp)
    price <-  as.numeric(Cl(mktdata[timestamp,]))
    posVal <- price * pos
    
    if(symbol == bondsym){
      
      iCol <- match.names(rankName,names(data))
      symRank <- data[timestamp,iCol]
      
      maxPosVal <- trunc(End.Eq/topN/100*(topN-symRank+1))*100
      maxQty <- trunc(maxPosVal / price /100)*100
      curQty <- pos
      orderqty <- maxQty-curQty
      
    }else{
      
      maxPosVal <- trunc(End.Eq/topN/100)*100
      maxQty <- trunc(maxPosVal / price /100)*100
      curQty <- pos
      orderqty <- max(maxQty-curQty,0)
      
    }
    
  }
  
  
  return(as.numeric(orderqty))
  
}



txnFUN <- function(TxnQty, TxnPrice, Symbol, pct = 0.03) {
  multiStock <- getInstrument(Symbol)$multiplier
  # Do something with multiStock, here it is equal to 1, so it's effectively meaningless but shows how you could go about using it.
  
  fees <- abs(TxnQty) * pct * multiStock
  # Fees are a negative deduction for the trade:
  if (fees > 0) fees <- -fees
  
  fees
}
