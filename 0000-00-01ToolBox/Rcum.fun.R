
Rcum.fun<-function(R, wealth.index = T, geometric = TRUE, 
                   colorset = (1:12), begin = c("first", "axis")){
  
  begin = begin[1]
  x = checkData(R)
  columns = ncol(x)
  columnnames = colnames(x)
  one = 0
  if (!wealth.index) 
    one = 1
  if (begin == "first") {
    length.column.one = length(x[, 1])
    start.row = 1
    start.index = 0
    while (is.na(x[start.row, 1])) {
      start.row = start.row + 1
    }
    x = x[start.row:length.column.one, ]
    if (geometric) 
      reference.index = na.skip(x[, 1], FUN = function(x) {
        cumprod(1 + x)
      })
    else reference.index = na.skip(x[, 1], FUN = function(x) {
      cumsum(x)
    })
  }
  for (column in 1:columns) {
    if (begin == "axis") {
      start.index = FALSE
    }
    else {
      start.row = 1
      while (is.na(x[start.row, column])) {
        start.row = start.row + 1
      }
      start.index = ifelse(start.row > 1, TRUE, FALSE)
    }
    if (start.index) {
      if (geometric) 
        z = na.skip(x[, column], FUN = function(x, index = reference.index[(start.row - 
                                                                              1)]) {
          rbind(index, 1 + x)
        })
      else z = na.skip(x[, column], FUN = function(x, index = reference.index[(start.row - 
                                                                                 1)]) {
        rbind(1 + index, 1 + x)
      })
    }
    else {
      z = 1 + x[, column]
    }
    column.Return.cumulative = na.skip(z, FUN = function(x, 
                                                         one, geometric) {
      if (geometric) 
        cumprod(x) - one
      else (1 - one) + cumsum(x - 1)
    }, one = one, geometric = geometric)
    if (column == 1) 
      Return.cumulative = column.Return.cumulative
    else Return.cumulative = merge(Return.cumulative, column.Return.cumulative)
  }
  if (columns == 1) 
    Return.cumulative = as.xts(Return.cumulative)
  colnames(Return.cumulative) = columnnames
  
  RR<-Return.cumulative
  RR
}