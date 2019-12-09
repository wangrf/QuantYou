
match.names<-function (match_names, data_names) 
{
  loc <- NULL
  for (mname in match_names) {
    t <- grep(mname, data_names)
    if (length(t) > 1) {
      t <- grep(paste(mname, "$", sep = ""), data_names)
    }
    if (is.null(loc)) 
      loc <- t
    else loc <- c(loc, t)
  }
  if (!identical(length(loc), length(match_names))) {
    mstr <- paste(match_names, collapse = " ")
    dstr <- paste(data_names, collapse = " ")
    warning(paste("all columns not located in", mstr, "for", 
                  dstr))
  }
  return(loc)
}
