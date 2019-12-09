
table.Weekly <- function(R, digits = 2, as.perc = TRUE, geometric = TRUE){
  
  ri = checkData(R, method = "zoo")
  columns = ncol(ri)
  columnnames = colnames(ri)
  rownames = rownames(ri)
  firstyear = as.numeric(format(strptime(as.POSIXct(time(ri)[1]), 
                                         "%Y-%m-%d"), "%Y"))
  lastyear = as.numeric(format(strptime(as.POSIXct(time(ri)[length(ri[, 
                                                                      1])]), "%Y-%m-%d"), "%Y"))
  year = format(strptime(as.POSIXct(time(ri)), "%Y-%m-%d"), 
                "%Y")
  month = format(strptime(as.POSIXct(time(ri)), "%Y-%m-%d"), 
                 "%W")
  monthlabels =c(paste0("0",0:9),10:53)
  rowlabels = (firstyear:lastyear)
  for (column in 1:columns) {
    target.df = as.data.frame(matrix(data = as.numeric(NA), 
                                     length(rowlabels), length(monthlabels), dimnames = list(rowlabels, 
                                                                                             monthlabels)))
    for (i in 1:length(ri[, 1])) {
      if (!is.na(ri[i, column])) {
        if(is.na(target.df[year[i],month[i]])){
          target.df[year[i], month[i]] = ri[i, column]
        }else{
          if(geometric){
            target.df[year[i], month[i]] = (1+target.df[year[i], month[i]] ) * (1+ri[i,column])-1
          }else{
            target.df[year[i], month[i]] = target.df[year[i], month[i]] + ri[i,column]
          }
        }
      }
    }
    yearcol = as.data.frame(matrix(data = as.numeric(NA), 
                                   length(rowlabels), 1, dimnames = list(rowlabels, 
                                                                         columnnames[column])))
    for (i in 1:length(yearcol[, 1])) {
      if (geometric) 
        yearcol[i, columnnames[column]] = prod(1 + na.omit(as.numeric(target.df[i, 
                                                                                ]))) - 1
      else yearcol[i, columnnames[column]] = sum(as.numeric(target.df[i, 
                                                                      ]), na.rm = TRUE)
      if (yearcol[i, columnnames[column]] == 0) 
        yearcol[i, columnnames[column]] = NA
    }
    target.df = cbind(target.df, yearcol)
    multiplier <- ifelse(as.perc,100,1)
    target.df = target.df * multiplier
    target.df = base::round(target.df, digits)
  
  }
  colnames(target.df) = c(paste0("week",monthlabels), columnnames)
  target.df
  
}