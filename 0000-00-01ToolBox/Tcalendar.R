Tcalendar<-function(x,daysInter="weekfirst"){
  
  
  
  a = .indexmon(x) + 1
  b = c(a[-1],13)
  f = c(1,diff(a))
  mon.last <- a != b
  mon.first <- f==1
  
  a = .indexweek(x)+1
  b=c(a[-1],8)
  f=c(1,diff(a))
  week.last <- a !=b
  week.first <- f==1
  
  day.every <- T
  
  Tcalendar <- (switch(daysInter,
                      monlast = mon.last,
                      monfirst = mon.first,
                      weeklast = week.last,
                      weekfirst = week.first,
                      dayevery = day.every))
  
  Tcalendar <- xts(Tcalendar,order.by=index(x))
  
  
  
return(Tcalendar)
  
}