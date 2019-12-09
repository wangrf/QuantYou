stgyPerformance <- function(prices,stgyname=""){
  

  if(stgyname==""){
    stgyname="strategy"
  }
  
  R.stgy = Return.calculate(prices,method="discrete")
  colnames( R.stgy)=stgyname
  tbmonth<-table.Monthly( R.stgy)
  tbweek<-table.Weekly(R.stgy)
  maxD<-maxDrawdown( R.stgy)
  tbYear<-table.AnnualizedReturns( R.stgy)
  print("monthly yield:")
  print(tbmonth)
  print("weekly yield")
  print(tbweek)
  print("maxDrawdown:")
  print(maxD)
  print("retrun summary:")
  print(tbYear)
  list(R.stgy=R.stgy,tbmonth=tbmonth,tbweek=tbweek,maxD=maxD,tbYear=tbYear)
  
}