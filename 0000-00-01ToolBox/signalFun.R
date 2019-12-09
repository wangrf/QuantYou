modify.args <- function(formals, arglist, ..., dots=FALSE)
{
  # avoid evaluating '...' to make things faster
  dots.names <- eval(substitute(alist(...)))
  
  if(missing(arglist))
    arglist <- NULL
  arglist <- c(arglist, dots.names)
  
  # see 'S Programming' p. 67 for this matching
  
  # nothing to do if arglist is empty; return formals
  if(!length(arglist))
    return(formals)
  
  argnames <- names(arglist)
  if(!is.list(arglist) && !is.null(argnames) && !any(argnames == ""))
    stop("'arglist' must be a *named* list, with no names == \"\"")
  
  .formals  <- formals
  onames <- names(.formals)
  
  pm <- pmatch(argnames, onames, nomatch = 0L)
  #if(any(pm == 0L))
  #    message(paste("some arguments stored for", fun, "do not match"))
  names(arglist[pm > 0L]) <- onames[pm]
  .formals[pm] <- arglist[pm > 0L]
  
  # include all elements from arglist if function formals contain '...'
  if(dots && !is.null(.formals$...)) {
    dotnames <- names(arglist[pm == 0L])
    .formals[dotnames] <- arglist[dotnames]
    #.formals$... <- NULL  # should we assume we matched them all?
  }
  
  .formals
}



sigAndx<-function (x = mktdata, columns, cross = FALSE) 
{
  ret_sig = NULL
  colNums <- rep(0, length(columns))
  for (i in 1:length(columns)) {
    colNums[i] <- match.names(columns[i], colnames(x))
  }
  ret_sig <- x[, colNums[1]]
  for (i in 2:length(colNums)) {
    ret_sig <- ret_sig & x[, colNums[i]]
  }
  ret_sig <- ret_sig * 1
  if (isTRUE(cross)) 
    ret_sig <- diff(ret_sig) == 1
  return(ret_sig)
}

sigThresholdx <- function (x, threshold = 0, relationship = c("gt", 
                                                                 "lt", "eq", "gte", "lte"), cross = FALSE) 
{
  
  relationship = relationship[1]
  ret_sig = NULL
  colNum <- 1
  
  
  switch(relationship, `>` = , gt = {
    ret_sig = x[, colNum] > threshold
  }, `<` = , lt = {
    ret_sig = x[, colNum] < threshold
  }, eq = {
    ret_sig = x[, colNum] == threshold
  }, gte = , gteq = , ge = {
    ret_sig = x[, colNum] >= threshold
  }, lte = , lteq = , le = {
    ret_sig = x[, colNum] <= threshold
  })
  if (isTRUE(cross)) 
    ret_sig <- diff(ret_sig) == 1
  
  return(ret_sig)
}

sigComparisionx<-function (x = mktdata, columns, relationship = c("gt", 
                                                           "lt", "eq", "gte", "lte"), offset1 = 0, offset2 = 0) 
{
  relationship = relationship[1]
  if (length(columns) == 2) {
    ret_sig = NULL
    if (relationship == "op") {
      if (columns[1] %in% c("Close", "Cl", "close")) 
        stop("Close not supported with relationship=='op'")
      switch(columns[1], Low = , low = , bid = {
        relationship = "lt"
      }, Hi = , High = , high = , ask = {
        relationship = "gt"
      })
    }
    colNums <- match.names(columns, colnames(x))
    opr <- switch(relationship, gt = , `>` = ">", lt = , 
                  `<` = "<", eq = , `==` = , `=` = "==", gte = , gteq = , 
                  ge = , `>=` = ">=", lte = , lteq = , le = , `<=` = "<=")
    ret_sig <- do.call(opr, list(x[, colNums[1]] + offset1, 
                                 x[, colNums[2]] + offset2))
  }
  else {
    stop("comparison of more than two columns not supported, see sigFormula")
  }
  
  return(ret_sig)
}
