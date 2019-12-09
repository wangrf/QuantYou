
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

osFixedDollar <- function(timestamp, orderqty, portfolio, symbol, ruletype, ...)
{
  pos <- getPosQty(portfolio, symbol, timestamp)
  if( isTRUE(all.equal(pos,0)) )
  {
    ClosePrice <- as.numeric(Cl(mktdata[timestamp,]))
    orderqty <- sign(orderqty)*round(tradeSize/ClosePrice,-2)
  } else {
    orderqty <- 0
  }
  return(orderqty)
}