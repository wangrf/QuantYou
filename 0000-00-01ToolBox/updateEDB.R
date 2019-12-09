


updateEDB <-
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
        indicators
      
      
      w_tdays_data<-w.tdays(endDate,endDate)$Data
      
      if(length(w_tdays_data)==0){
        endDate=w.tdaysoffset(0,endDate)$Data[1,1]
        
      }
      
      
      y <-
        w.edb(new.ind,
              startDate,
              endDate,
              "Fill=Previous")$Data
      y <- as.xts(y[,-1], order.by = as.Date(y[[1]], "%Y-%m-%d"))
      indexFormat(y) <- '%Y-%m-%d'
      indexTZ(y) <- "UTC"
      colnames(y)<-new.ind
      
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
      x<-x[1:(nrow(x)-20),]
      
      last.date <- as.Date(tail(index(x), 1), "%Y-%m-%d")
      
      tempS <-
        sapply(names(x), function(y)
          max(unlist(gregexpr(
            "[.]", y, useBytes = T
          )))) + 1
      tempE <- sapply(names(x), nchar)
      idx.name <-
        unlist(mapply(
          substr,
          x = names(x),
          start = tempS,
          stop = tempE
        ))
      
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
          w.edb(idx.name,
                startDate,
                endDate,
                "Fill=Previous")$Data
        y <- as.xts(y[,-1], order.by = as.Date(y[[1]], "%Y-%m-%d"))
        indexFormat(y) <- '%Y-%m-%d'
        indexTZ(y) <- "UTC"
        
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
        loadRData(sym)
        
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
            w.edb( new.ind, sDate, eDate)$Data
          
          
          y <-
            as.xts(y[,-1], order.by = as.Date(y[[1]], "%Y-%m-%d"))
          
          indexFormat(y) <- '%Y-%m-%d'
          indexTZ(y) <- "UTC"
          colnames(y) <- new.ind
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
