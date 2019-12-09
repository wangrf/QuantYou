


updateStock <-
  function(sym,
           startDate,
           endDate,
           indicators,
           dataPath = dataPath) {
    data.list <- dir(pattern = ".RData", path = dataPath)
    idMatch <- grep(paste0(sym,".RData"), data.list)
    nMatch <- length(idMatch)
    
    if (nMatch == 0) {
      warning(paste0(sym, " No history data and it is the first time"))
      
      startDate <- as.Date(startDate, "%Y-%m-%d")
      endDate <- as.Date(endDate, "%Y-%m-%d")
      new.ind <-
        union(c("open", "high", "low", "close"), indicators)
      
      w_tdays_data<-w.tdays(endDate,endDate)$Data
      
      if(length(w_tdays_data)==0){
        endDate=w.tdaysoffset(0,endDate)$Data[1,1]
        
      }
      
      
      y <-
        w.wsd(sym,
              new.ind,
              startDate,
              endDate,
              "Currency=CNY;PriceAdj=F;Fill=Previous")$Data
      y <- as.xts(y[,-1], order.by = as.Date(y[[1]], "%Y-%m-%d"))
      indexFormat(y) <- '%Y-%m-%d'
      indexTZ(y) <- "UTC"
      idx.open <- match.names("OPEN", colnames(y))
      idx.high <- match.names("HIGH", colnames(y))
      idx.low <- match.names("LOW", colnames(y))
      idx.close <- match.names("CLOSE", colnames(y))
      y[, idx.close] <-
        ifelse(is.na(y[, idx.close]), rowMeans(y[, c(idx.open, idx.high, idx.low)], na.rm = T), y[, idx.close])
      y[, idx.close] <-
        ifelse(is.na(y[, idx.close]), 0, y[, idx.close])
      y[, idx.high] <-
        ifelse(is.na(y[, idx.high]), y[, idx.close], y[, idx.high])
      y[, idx.low] <-
        ifelse(is.na(y[, idx.low]), y[, idx.close], y[, idx.low])
      y[, idx.open] <-
        ifelse(is.na(y[, idx.open]), y[, idx.close], y[, idx.open])
      
      colnames(y) <- paste0(sym, ".", new.ind)
      
      assign(sym, y)
      save(list = c(sym),
           file = paste0(dataPath, "/", sym, ".RData"))
      print(
        paste0(
          "Have updated ",
          paste0(sym, collapse = ","),
          "",
          "Have updated the indicators of ",
          paste0(new.ind, collapse = ","),
          ",and update the date to",
          endDate
        )
      )
      
      
    
      }else if (nMatch > 1) {
      stop(paste0("Exits multiple RData", "for ", sym))
    }else if (nMatch==1){
      load(paste0(dataPath, "/", data.list[idMatch]), envir = .GlobalEnv)
      
      x <- NULL
      x <- get(sym)
      last.date <- as.Date(tail(index(x), 1), "%Y-%m-%d")
      
      tempS <-
        sapply(names(x), function(y)
          max(unlist(gregexpr(
            "[.]", y, useBytes = T
          )))) + 1
      tempE <- sapply(names(x), nchar)
      idx.name <-
        tolower(unlist(mapply(
          substr,
          x = names(x),
          start = tempS,
          stop = tempE
        )))
      
      old.ind <- idx.name
      new.ind <- setdiff(indicators, idx.name)
      
      w_tdays_data<-w.tdays(endDate,endDate)$Data
      
      if(length(w_tdays_data)==0){
        endDate=w.tdaysoffset(0,endDate)$Data[1,1]
        
      }
      
        
        
        if ((last.date < as.Date(endDate, "%Y-%m-%d"))) {
          last.date <- as.Date(tail(index(x), 1), "%Y-%m-%d")
          startDate <- as.Date(startDate, "%Y-%m-%d")
          endDate <- as.Date(endDate, "%Y-%m-%d")
          
          startDate <- last.date + 1
          
          ### update the date
          y <-
            w.wsd(sym,
                  idx.name,
                  startDate,
                  endDate,
                  "Currency=CNY;PriceAdj=F")$Data
          
          y <- as.xts(y[,-1], order.by = as.Date(y[[1]], "%Y-%m-%d"))
          indexFormat(y) <- '%Y-%m-%d'
          indexTZ(y) <- "UTC"
          idx.open <- match.names("OPEN", colnames(y))
          idx.high <- match.names("HIGH", colnames(y))
          idx.low <- match.names("LOW", colnames(y))
          idx.close <- match.names("CLOSE", colnames(y))
          y[, idx.close] <-
            ifelse(is.na(y[, idx.close]), rowMeans(y[, c(idx.open, idx.high, idx.low)], na.rm = T), y[, idx.close])
          y[, idx.close] <-
            ifelse(is.na(y[, idx.close]), 0, y[, idx.close])
          y[, idx.high] <-
            ifelse(is.na(y[, idx.high]), y[, idx.close], y[, idx.high])
          y[, idx.low] <-
            ifelse(is.na(y[, idx.low]), y[, idx.close], y[, idx.low])
          y[, idx.open] <-
            ifelse(is.na(y[, idx.open]), y[, idx.close], y[, idx.open])
          names(y) <- names(x)
          x <- rbind(x, y)
          
          assign(sym, x)
          save(list = c(sym),
               file = paste0(dataPath, "/", sym, ".RData"))
          print(
            paste0(
              "Have updated",
              paste0(sym, collapse = ","),
              "data",
              ",Have updated the indicators of ",
              paste0(old.ind, collapse = ","),
              ",and update the date to",
              endDate
            )
          )
          
          
        }
        
        if (length(new.ind) > 0) {
          load(paste0(dataPath, "/", sym, ".RData"), envir = .GlobalEnv)
          
          last.date <- as.Date(tail(index(x), 1), "%Y-%m-%d")
          startDate <- as.Date(startDate, "%Y-%m-%d")
          endDate <- as.Date(endDate, "%Y-%m-%d")
          
          w_tdays_data<-w.tdays(endDate,endDate)$Data
          
          if(length(w_tdays_data)==0){
            endDate=w.tdaysoffset(0,endDate)$Data[1,1]
            
          }
          
          ## update the indicators
          if (length(new.ind) > 0) {
            sDate <- head(index(x), 1)
            eDate <- tail(index(x), 1)
            y <-
              w.wsd(sym, new.ind, sDate, eDate, "Currency=CNY;PriceAdj=F")$Data
            y <-
              as.xts(y[,-1], order.by = as.Date(y[[1]], "%Y-%m-%d"))
            indexFormat(y) <- '%Y-%m-%d'
            indexTZ(y) <- "UTC"
            colnames(y) <- paste0(sym, ".", new.ind)
            x <- cbind(x, y)
            
          }
          
          assign(sym, x)
          save(list = c(sym),
               file = paste0(dataPath, "/", sym, ".RData"))
          print(
            paste0(
              "Have updated",
              paste0(sym, collapse = ","),
              "data",
              ",Have updated the indicators of ",
              paste0(new.ind, collapse = ","),
              ",and update the date to",
              endDate
            )
          )
          
        }
        
      
      
    }else{
      print(paste0("There is no need to update,the ", sym, " is the newest"))
    }
  }
